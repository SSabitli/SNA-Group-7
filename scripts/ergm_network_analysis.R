# --------------------------------------------------------------------------- #
# NOTE: NOT THE LATEST ERGM ANALYSIS! FOR REFERENCE ONLY

# If not already Installed
install.packages("viridis")     # For Colours
install.packages("Rglpk")       # Additional solver for ERGMs
install.packages("here")        # To locate files from RProj

# Import the Network and Other Object
ergm_path <- "resources/objects/ergm/"
bondora_net <- readRDS(here::here(
  "resources","objects","preprocessing","bondora_net.Rds"))
b_indicator <- readRDS(here::here(
  "resources","objects","preprocessing","indicator.Rds"))

# Set colour palette
cols <- viridis::viridis(30)

# Determine acceptable core count
n_cores <- parallel::detectCores() - 3 # Leave some out for other processes
print(paste("You have",n_cores,"usable cores"))

# Repeatability
seed(42)

# Save plots
save_ergm_plot <- function(plt_nam) {
  plt <- recordPlot()
  saveRDS(plt, here::here("resources","objects","ergm",
                          paste0(plt_nam,".Rds")))
}
# --------------------------------------------------------------------------- #
# Copy network for plotting
bondora_plot <- bondora_net

# Get node type for plotting
type_indicator <- ifelse(b_indicator == 2, TRUE,FALSE)
shape <- ifelse(type_indicator,"square","circle")
network::set.vertex.attribute(bondora_plot, "shape", shape)

# Get Category Count for Vertex Size
counts <- sna::degree(bondora_plot)
counts_att <- ifelse(type_indicator, log(counts)*4, counts*1.5)
network::set.vertex.attribute(bondora_plot, "size", counts_att)

# Colours for the Node Types
plot_cols <- ifelse(type_indicator, cols[5], cols[30])
network::set.vertex.attribute(bondora_plot, "color", plot_cols)

# Legend Plotting
type_legend <- ifelse(type_indicator, "Borrowers", "Loan Type")
type_legend <- as.factor(type_legend)

# Plot the Network
plot(snafun::to_igraph(bondora_plot),
     #main = "Bipartite User-LoanUse",
     edge.arrow.size = 0.3,
     edge.color = rgb(0,0,0, alpha = 0.35),
     vertex.frame.color = "black",
     vertex.label = NA,
     vertex.frame.size = 3,
     edge.curved = FALSE,
     layout=igraph::layout.fruchterman.reingold)
legend("bottomleft", 
       legend = levels(type_legend), 
       inset = c(0.15, 0.01),
       col = c(cols[30], cols[5]),
       pch = c(16, 15), 
       title = "Node Partitions", 
       title.font = 2,
       cex = 1,              # Increase the text size
       pt.cex = 2,             # Increase the point symbol size
       box.lwd = 1,            # Thin box border
       box.col = "black",      # Box color
       bty = "o"               # Use a box around legend
)
save_ergm_plot("network_plot")

# Summary Statistics
snafun::g_density(bondora_net)
snafun::g_centralize(bondora_net)

# Plot Network Summary Statistics
snafun::plot_centralities(bondora_net)
save_ergm_plot("network_plots")
# --------------------------------------------------------------------------- #
# Make function to calculate probabilities from log odds
lodds_to_prob <- function(l_odd) {
  return(exp(l_odd) / (1 + exp(l_odd)))
}
# Make function to save ERGM object
save_ergm <- function(object, id) {
  saveRDS(object, file=here::here(
    "resources","objects","ergm",id,".Rds"))
}
# Make function to conduct ERGMs automatically
auto_ergm <- function(model, mcmc, name) {
  
  # Conducts the GOF Diagnostics and then saves the model,
  # mcmc diagnostics and gof object in a list.
  # This list can be imported as an .RDS object into the R environment
  
  # Diagnostics
  if (mcmc) {
    ergm::mcmc.diagnostics(model)
  }
  
  # The GOF must be adjusted otherwise it takes too long
  # We do not limit the GOF by changing its range of parameters
  gof <- ergm::gof(model,
                   control = ergm::control.gof.ergm(
                     nsim = 200,
                     MCMC.burnin = 5000,
                     MCMC.interval = 1000,
                     parallel = n_cores,
                     parallel.type = "PSOCK"
                   ))
  
  # Return List to view each item separately
  result <- list(model, gof)
  names(result) <- c("model","gof")
  save_ergm(result, paste0(name,"_panel"))
  
  return(result)
}
# --------------------------------------------------------------------------- #
# Find max degree
(max_deg <- max(summary(bondora_net ~ b2factor("b2_loantype"))))
# --------------------------------------------------------------------------- #
# Base Model + GOF
formula_base_model <- bondora_net ~ edges
base_ergm <- ergm::ergm(formula_base_model)
base_ergm_panel <- auto_ergm(base_ergm, mcmc = FALSE, name = "ergm_base")
snafun::stat_plot_gof(base_ergm_panel$gof) 
models = list(base_ergm)
texreg::screenreg(models)
# --------------------------------------------------------------------------- #
# Base Model + Edge Counts + GOF
#base_model_counts <- ergm::ergm(bondora_net ~ edges, response="frequency",
#                                reference = ~ Poisson)
#basemodel_counts_gof <- ergm::gof(base_model_counts)
#snafun::stat_plot_gof(basemodel_counts_gof)
#
#texreg::screenreg(list(base_model, base_model_counts))
# --------------------------------------------------------------------------- #
# Iteration 1 + MCMC Diagnostics + GOF
model_1_params <- bondora_net ~ edges + 
  # b1 decay can be very low since 9 b2
  gwb1degree(decay=0.15, fixed=TRUE) 

model_1 <- ergm::ergm(
  model_1_params,
  
  # Max b2 degree is 72, so this constraint is reasonable
  # and helps convergence significantly.
  # Technically in the Bondora population this can be 
  # far higher but we are studying a subsample.
  #constraints = ~ bd(minout = 0, maxout = 80),
  
  control = ergm::control.ergm(
    # Greater burn-in for cleaner result
    MCMC.burnin = 20000,
    # Greater sample size for greater stability
    MCMC.samplesize = 100000,
    seed = 42,
    MCMC.interval = 1000,
    # Only needed for convergence pvals to improve
    MCMLE.maxit = 45,
    # Smaller steps for stability
    MCMLE.steplength = 0.25,
    parallel = n_cores,
    parallel.type = "PSOCK"
  )
)

model_1_panel <- auto_ergm(model=model_1, mcmc=TRUE, name="ergm_m1")
model_1_panel$gof
snafun::stat_plot_gof(model_1_panel$gof) 
texreg::screenreg(list(base_ergm, model_1))
# --------------------------------------------------------------------------- #
# Iteration 2 + MCMC Diagnostics + GOF
model_2_params <- bondora_net ~ edges + 
  # low decay important because there is high clustering around low degrees
  gwb1degree(decay=0.15, fixed=TRUE) + 
  # decay should be higher due to wider variation in degree but 
  # too high of degree makes the traces concentrated around the tails.
  gwb1dsp(decay=0.5, fixed=TRUE)      
  
model_2 <- ergm::ergm(
  model_2_params,
  
  # Max b2 degree is 72, so this constraint is reasonable
  # and helps convergence significantly.
  # Technically in the Bondora population this can be 
  # far higher but we are studying a subsample.
  constraints = ~ bd(minout = 0, maxout = 80),
  
  control = ergm::control.ergm(
    # Greater burn-in for cleaner result
    MCMC.burnin = 20000,
    # Greater sample size for greater stability
    MCMC.samplesize = 100000,
    seed = 42,
    MCMC.interval = 1000,
    # Only needed for convergence pvals to improve
    MCMLE.maxit = 45,
    # Smaller steps for stability
    MCMLE.steplength = 0.25,
    parallel = n_cores,
    parallel.type = "PSOCK"
    )
  )

model_2_panel <- auto_ergm(model=model_2, mcmc=TRUE, name="ergm_m2")
snafun::stat_plot_gof(model_2_panel$gof) 
model_2_panel$gof
models <- list(base_ergm, model_1, model_2)
texreg::screenreg(models)
# --------------------------------------------------------------------------- #
# Iteration 3 + MCMC Diagnostics + GOF
model_3_params <- bondora_net ~ edges + 
  # low decay important because there is high clustering around low degrees
  gwb1degree(decay=0.15, fixed=TRUE) + 
  # decay should be higher due to wider variation in degree but 
  # too high of degree makes the traces concentrated around the tails.
  gwb1dsp(decay=0.5, fixed=TRUE) +
  # See differences across genders (implicitly, since b1nodemix unavailable)
  b1nodematch("b1_gender", diff=FALSE)

model_3 <- ergm::ergm(
  model_3_params,
  
  # Max b2 degree is 72, so this constraint is reasonable
  # and helps convergence significantly.
  # Technically in the Bondora population this can be 
  # far higher but we are studying a subsample.
  constraints = ~ bd(minout = 0, maxout = 80),
  
  control = ergm::control.ergm(
    # Greater burn-in for cleaner result
    MCMC.burnin = 20000,
    # Greater sample size for greater stability
    MCMC.samplesize = 100000,
    seed = 42,
    MCMC.interval = 1000,
    # Only needed for convergence pvals to improve
    MCMLE.maxit = 45,
    # Smaller steps for stability
    MCMLE.steplength = 0.25,
    parallel = n_cores,
    parallel.type = "PSOCK"
  )
)

model_3_panel <- auto_ergm(model=model_3, mcmc=TRUE, name="ergm_m3")
snafun::stat_plot_gof(model_3_panel$gof) 
model_3_panel$gof
models <- list(base_ergm, model_1, model_2, model_3)
texreg::screenreg(models)
# --------------------------------------------------------------------------- #
# Iteration 4 + MCMC Diagnostics + GOF
model_4_params <- bondora_net ~ edges + 
  # low decay important because there is high clustering around low degrees
  gwb1degree(decay=0.15, fixed=TRUE) + 
  # decay should be higher due to wider variation in degree but 
  # too high of degree makes the traces concentrated around the tails.
  gwb1dsp(decay=0.5, fixed=TRUE) +
  # See differences across genders (implicitly, since b1nodemix unavailable)
  b1nodematch("b1_gender", diff=TRUE) +
  # See if higher ages make a difference
  b1cov("b1_age")

model_4 <- ergm::ergm(
  model_4_params,
  
  # Max b2 degree is 72, so this constraint is reasonable
  # and helps convergence significantly.
  # Technically in the Bondora population this can be 
  # far higher but we are studying a subsample.
  constraints = ~ bd(minout = 0, maxout = 80),
  
  control = ergm::control.ergm(
    # Greater burn-in for cleaner result
    MCMC.burnin = 20000,
    # Greater sample size for greater stability
    MCMC.samplesize = 100000,
    seed = 42,
    MCMC.interval = 1000,
    # Only needed for convergence pvals to improve
    MCMLE.maxit = 45,
    # Smaller steps for stability
    MCMLE.steplength = 0.25,
    parallel = n_cores,
    parallel.type = "PSOCK"
  )
)

model_4_panel <- auto_ergm(model=model_4, mcmc=TRUE, name="ergm_m4")
model_4_panel$gof
snafun::stat_plot_gof(model_4_panel$gof) 
models <- list(base_ergm, model_1, model_2, model_3, model_4)
texreg::screenreg(models)
# --------------------------------------------------------------------------- #
