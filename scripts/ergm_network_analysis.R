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
set.seed(42)

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
counts_att <- ifelse(type_indicator, log(counts)*4, counts*2.5)
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

# Summary Statistics and Save Them
density <- snafun::g_density(bondora_net)[1]
centralization <- snafun::g_centralize(bondora_net)[1]
vertices <- snafun::count_vertices(bondora_net)[1]
edges <- snafun::count_edges(bondora_net)[1]
dist <- snafun::g_mean_distance(bondora_net)[1]

net_names <- c("Vertex Count","Edge Count","Density","Centralization",
               "Mean Distance")
net_stats <- c(vertices, edges, density, centralization, dist)
net_stats <- sapply(net_stats, function(x) round(as.numeric(x),2))

net_summary <- data.frame(Statistic = net_names,
                          "Measure" = net_stats)
knitr::kable(net_summary)
saveRDS(net_summary, here::here("resources","objects","ergm","net_summary.Rds"))

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
    "resources","objects","ergm",paste0(id,".Rds")))
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
# Make Function to do Coefficient Plot
plot_coef <- function(model,
                      model_name = "Model Estimates", 
                      x_axis_label = "Coefficient Estimate") {
  
  # Extract Coefficients and SEs from ERGM 
  coefs <- coef(model)
  ses <- sqrt(diag(vcov(model)))
  
  # Make DF
  df <- data.frame(
    term = names(coefs),
    estimate = coefs,
    se = ses
  )
  
  # 95% Conf. Interval calculation (z-score 1.96)
  df$lower <- df$estimate - 1.96 * df$se
  df$upper <- df$estimate + 1.96 * df$se
  
  # Significance indicator: CI does not cross zero
  df$sig <- df$lower * df$upper > 0
  df$sig <- as.character(df$sig) # Convert to character to make it work
  
  # 3. Reorder terms by estimate (Base R factor reordering)
  order_index <- order(df$estimate)
  df$term <- factor(df$term, levels = df$term[order_index])
  
  # 4. Create the ggplot (Uses ggplot2:: explicitly, as requested)
  ggplot2::ggplot(df, ggplot2::aes(x = estimate, y = term)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper), 
                            height = 0.2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_bw() + 
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(), 
      panel.grid.minor.x = ggplot2::element_blank()) +
    ggplot2::labs(
      x = x_axis_label,
      y = "Variable",
      title = model_name
      #color = "Significance"
    )
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
# Iteration 1 + MCMC Diagnostics + GOF
model_1_params <- bondora_net ~ edges + 
  # See if individuals tend to pick one loan type
  b1degree(1) 

model_1 <- ergm::ergm(
  model_1_params,
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

# Plot Coefficients
plot_coef(ergm_m1$model, model_name = "Baseline + b1degree")
# --------------------------------------------------------------------------- #
# Iteration 2 + MCMC Diagnostics + GOF
model_2_params <- bondora_net ~ edges + b1degree(1) + 
  # See if there is clustering around pairs of loan types
  cycle(4)
  
model_2 <- ergm::ergm(
  model_2_params,
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

model_2_panel <- auto_ergm(model=model_2, mcmc=TRUE, name="ergm_m2")
snafun::stat_plot_gof(model_2_panel$gof) 
model_2_panel$gof
models <- list(base_ergm, model_1, model_2)
texreg::screenreg(models)

# Plot Coefficients
plot_coef(model_2_panel$model, model_name = "Baseline + b1degree + cycle(4)")
# --------------------------------------------------------------------------- #
# Iteration 3 + MCMC Diagnostics + GOF
model_3_params <- bondora_net ~ edges + b1degree(1) + cycle(4) +
  
  # See if there is gender effect
  b1factor("b1_gender")

model_3 <- ergm::ergm(
  model_3_params,
  #constraints = ~ bd(minout = 0, maxout = 80),
  
  control = ergm::control.ergm(
    # Greater burn-in for cleaner result
    # Cycles mixed with the rest of the terms requires more burnin samples
    MCMC.burnin = 20000,
    # Greater sample size for greater stability
    MCMC.samplesize = 100000,
    seed = 42,
    # Reducing interval to reduce complexity between intervals
    MCMC.interval = 1000,
    # Only needed for convergence pvals to improve
    MCMLE.maxit = 25,
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

# Plot Coefficients
plot_coef(model_3_panel$model,
          model_name = "Baseline + b1degree + cycle(4) + b1factor(gender)")
# --------------------------------------------------------------------------- #
# Iteration 4 + MCMC Diagnostics + GOF
model_4_params <- bondora_net ~ edges + b1degree(1) + cycle(4) +
  b1factor("b1_gender") +
  
  # See if higher ages make a difference
  b1cov("b1_age")

model_4 <- ergm::ergm(
  model_4_params,
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

model_4_panel <- auto_ergm(model=model_4, mcmc=TRUE, name="ergm_m4")
model_4_panel$gof
snafun::stat_plot_gof(model_4_panel$gof) 
models <- list(base_ergm, model_1, model_2, model_3, model_4)
texreg::screenreg(models)

# Plot Coefficients
m4_ergm_coef_plot <- plot_coef(model_4_panel$model,
          model_name = "Complete Model")
# Save Plot
ggplot2::ggsave("mr_coef_plot.png",
                plot = m4_ergm_coef_plot,
                device = "png",
                path = here::here("resources","objects","ergm"))
# --------------------------------------------------------------------------- #
# Collect Models in Table 1: Raw results
headers <- c("Base ERGM","b1degree(1)","cycle(4)","Age","Gender")
texreg::knitreg(models, custom.model.names = headers,
                                digits = 3)

# Save the Table to use in Report
saveRDS(list(models, headers),
        here::here("resources","objects","ergm","ergm_table.Rds"))

# Padding
pad <- function(x, max) {
  return(c(unname(x), rep("-", max-length(x))))
}

# Collect Models in Table 2: Log Odds Results
max_ergms <- length(model_4$coefficients)
df_prob <- data.frame(
  Terms = names(model_4$coefficients),
  "Base" = pad(round(lodds_to_prob(base_ergm$coefficients),3),max_ergms),
  "b1degree(1)" = pad(round(lodds_to_prob(model_1$coefficients),3),max_ergms),
  "cycle(4)" = pad(round(lodds_to_prob(model_2$coefficients),3),max_ergms),
  "Age" = pad(round(lodds_to_prob(model_3$coefficients),3),max_ergms),
  "Gender" = pad(round(lodds_to_prob(model_4$coefficients),3),max_ergms)
)
knitr::kable(df_prob)

# Save Table to use in Report
saveRDS(df_prob, 
        here::here("resources","objects","ergm","ergm_prob_tables.Rds"))
# --------------------------------------------------------------------------- #
