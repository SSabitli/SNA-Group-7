# --------------------------------------------------------------------------- #
# If not already Installed
install.packages("here") # To locate files within directory easier
install.packages("viridis") # For Colours
install.packages("ergm.count")

# Import the Network and Other Object
bondora_net <- readRDS(here::here("resources","objects","bondora_net.RDS"))

# Set colour palette
cols <- viridis::viridis(30)
# --------------------------------------------------------------------------- #
# Copy network for plotting
bondora_plot <- bondora_net

# Get node type for plotting
type <- network::get.vertex.attribute(bondora_net, "bipartite")
type_indicator <- is.na(type)
shape <- ifelse(type_indicator,"square","circle")
network::set.vertex.attribute(bondora_plot, "shape", shape)

# Get Category Count for Vertex Size
counts <- summary(bondora_plot ~ b2sociality)
counts_att <- ifelse(type_indicator, counts^0.75, counts^0.6)
network::set.vertex.attribute(bondora_plot, "size", counts_att)

# Colours for the Node Types
plot_cols <- ifelse(type_indicator, cols[5], cols[30])
network::set.vertex.attribute(bondora_plot, "color", plot_cols)

# Legend Plotting
type_legend <- ifelse(type_indicator, "Borrowers", "Loan Type")
type_legend <- as.factor(type_legend)

# Plot the Network
plot(snafun::to_igraph(bondora_plot),
     main = "Bipartite User-LoanUse",
     edge.arrow.size = 0.3,
     edge.color = rgb(0,0,0, alpha = 0.35),
     vertex.frame.color = "black",
     vertex.label = NA,
     vertex.frame.size = 3,
     edge.curved = FALSE,
     layout=igraph::layout.fruchterman.reingold)
legend("bottomleft", legend = levels(type_legend), 
       inset = c(0.1, 0.02),
       col = c(cols[5], cols[30]),
       pch = c(16,15), 
       title = "Node Partitions", title.font = 2,
       cex = 0.8,       
       lwd = 1,
       bg = rgb(0,0,0, alpha=0.025))

# Summary Statistics
snafun::g_density(bondora_net)
snafun::g_centralize(bondora_net)

# Degree and Betweenness Distribution
deg_dist <- snafun::g_degree_distribution(bondora_net)
bet_dist <- snafun::v_betweenness(bondora_net)

# Other Descriptives
summary(bondora_net ~ b1degree(1:9))
summary(bondora_net ~ b2degree(1:10))

par(mfrow = c(1,2))

hist(deg_dist,
     main = "Unadjusted Degree Distribution",
     xlab = "Node Degree",
     col = cols[1],
     border = "cornsilk4")

hist(bet_dist,
     main = "Unadjusted Betweenness",
     xlab = "Betweenness",
     col = cols[1],
     border = "cornsilk4")

par(mfrow = c(1,1))
# --------------------------------------------------------------------------- #
# Make function to calculate probabilities from log odds
lodds_to_prob <- function(l_odd) {
  return(exp(l_odd) / (1 + exp(l_odd)))
}
# Make function to save ERGM object
save_ergm <- function(object, id) {
  saveRDS(object, file=here::here("resources","objects",id))
}
# Make function to conduct ERGMs automatically
auto_ergm <- function(model, mcmc = TRUE, name, compare) {

  # Diagnostics
  if (mcmc) {
    mcmc_diags <- ergm::mcmc.diagnostics(model)
    gof <- ergm::gof(model)
  } else {
    mcmc_diags <- NULL
    gof <- ergm::gof(model)
  }
  
  # Save Model
  save_ergm(model, name)
  save_ergm(gof, paste(name,"_gof"))
  
  # Compare to other Models
  updated_comps <- append(compare, list(model))
  comps <- texreg::screenreg(updated_comps)
  
  # Return List to view each item separately
  result <- list(mcmc_diags, gof, comps)
  names(result) <- c("mcmc","gof","comps")
  
  return(result)
}
# --------------------------------------------------------------------------- #
# Find max degree
(max_deg <- max(summary(bondora_net ~ b2factor("b2_loantype"))))
# --------------------------------------------------------------------------- #
# Base Model + GOF
formula_base_model <- bondora_net ~ edges
base_ergm <- ergm::ergm(formula_base_model)
base_ergm_panel <- auto_ergm(base_ergm, mcmc = FALSE,
                             name = "ergm_base", compare = list())
snafun::stat_plot_gof(base_ergm_panel$gof) 
base_ergm_panel$comps
models = list(base_ergm)
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
model_1_params <- bondora_net ~ edges + gwb1dsp(decay=0.1, fixed=TRUE)
model_1 <- ergm::ergm(model_1_params, 
                      constraints= ~ bd(minout = 0, maxout = max_deg),
                      control = ergm::control.ergm(
      MCMC.burnin = 10000,
      MCMC.samplesize = 50000,
      seed = 42,
      MCMLE.maxit = 20,
      parallel = 12,
      parallel.type = "PSOCK"
    )
  )
model_1_panel <- auto_ergm(model=model_1, mcmc=TRUE,
                           name="ergm_m1",compare=models)
model_1_panel$mcmc
snafun::stat_plot_gof(model_1_panel$gof) 
model_1_panel$comps
models <- append(models, model_1)
# --------------------------------------------------------------------------- #
# Iteration 2 + MCMC Diagnostics + GOF
model_2_params <- bondora_net ~ edges + gwb1dsp(decay=0.1, fixed=TRUE) +
  gwb2dsp(decay=0.1, fixed=TRUE)
model_2 <- ergm::ergm(model_2_params, 
                      constraints= ~ bd(minout = 0, maxout = max_deg),
                      control = ergm::control.ergm(
  MCMC.burnin = 10000,
  MCMC.samplesize = 100000,
  seed = 42,
  MCMLE.maxit = 20,
  parallel = 12,
  parallel.type = "PSOCK"
    )
  )
model_2_panel <- auto_ergm(model=model_2, mcmc=TRUE,
                           name="ergm_m2", compare=models)
snafun::stat_plot_gof(model_2_panel$gof) 
model_2_panel$comps
models <- append(models, model_2)
# --------------------------------------------------------------------------- #
# Iteration 3 + MCMC Diagnostics + GOF
model_3_params <- bondora_net ~ edges + gwb1dsp(decay=0.1, fixed=TRUE) +
  gwb2dsp(decay=0.1, fixed=TRUE) + b1nodematch("b1_gender")
model_3 <- ergm::ergm(model_3_params, 
                      constraints= ~ bd(minout = 0, maxout = max_deg),
                      control = ergm::control.ergm(
                        MCMC.burnin = 10000,
                        MCMC.samplesize = 100000,
                        seed = 42,
                        MCMLE.maxit = 20,
                        parallel = 12,
                        parallel.type = "PSOCK"
                      )
  )
model_3_panel <- auto_ergm(model=model_3, mcmc=TRUE,
                           name="ergm_m3", compare=models)
snafun::stat_plot_gof(model_3_panel$gof) 
model_3_panel$comps
models <- append(models, model_3)
# --------------------------------------------------------------------------- #
# Iteration 4 + MCMC Diagnostics + GOF
model_4_params <- bondora_net ~ edges + gwb1dsp(decay=0.1, fixed=TRUE) +
  gwb2dsp(decay=0.1, fixed=TRUE) + b2factor("b2_loantype")
model_4 <- ergm::ergm(model_4_params, 
                      constraints= ~ bd(minout = 0, maxout = max_deg),
                      control = ergm::control.ergm(
                        MCMC.burnin = 10000,
                        MCMC.samplesize = 100000,
                        seed = 42,
                        MCMLE.maxit = 20,
                        parallel = 12,
                        parallel.type = "PSOCK"
                      )
)
model_4_panel <- auto_ergm(model=model_4, mcmc=TRUE,
                           name="ergm_m4", compare=models)
snafun::stat_plot_gof(model_4_panel$gof) 
model_4_panel$comps
models <- append(models, model_4)
# --------------------------------------------------------------------------- #
