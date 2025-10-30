# --------------------------------------------------------------------------- #
# If not already Installed
install.packages("here")

# Import the Network and Other Object
bondora_net <- readRDS(here::here("resources","objects","bondora_net.RDS"))

# Set colour palette
cols <- RColorBrewer::brewer.pal(n = 3, name = "RdBu")
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
counts_att <- ifelse(type_indicator, counts^0.75, counts^0.5)
network::set.vertex.attribute(bondora_plot, "size", counts_att)

# Colours for the Node Types
plot_cols <- ifelse(type_indicator, cols[3], cols[1])
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
legend("bottomleft", legend=levels(type_legend), col=c(cols[1], cols[3]), 
       box.lwd=1, box.lty=1, bg=rgb(0,0,0, alpha=0.025),
       pt.cex=2, lwd = 1, pch=c(16,15), title = "Node Partitions", 
       title.font=2)

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
# Base Model + GOF
base_model <- ergm::ergm(p2p_network ~ edges)
basemodel_gof <- ergm::gof(base_model)
snafun::stat_plot_gof(basemodel_gof)

texreg::screenreg(base_model)

# Iteration 1 + MCMC Diagnostics + GOF
model_1_params <- bondora_net ~ edges + b1degree(1)
model_1 <- ergm::ergm(model_1_params, 
                      constraints= ~ bd(minout = 0, maxout = 10),
                      control = ergm::control.ergm(
      MCMC.burnin = 10000,
      MCMC.samplesize = 50000,
      seed = 42,
      MCMLE.maxit = 10,
      parallel = 10,
      parallel.type = "PSOCK"
    )
  )
ergm::mcmc.diagnostics(model_1)
model_1_gof <- ergm::gof(model_1)
snafun::stat_plot_gof(model_1_gof)
model_1_gof

models <- list(base_model, model_1)
texreg::screenreg(models)
# --------------------------------------------------------------------------- #
# Iteration 2 + MCMC Diagnostics + GOF
model_2_params <- bondora_net ~ edges + gwb1dsp(decay=0.05, fixed=TRUE) +
  gwb2dsp(decay=0.05, fixed=TRUE)
model_2 <- ergm::ergm(model_2_params, 
                      constraints= ~ bd(minout = 0, maxout = 20),
                      control = ergm::control.ergm(
  MCMC.burnin = 10000,
  MCMC.samplesize = 50000,
  seed = 42,
  MCMLE.maxit = 10,
  parallel = 10,
  parallel.type = "PSOCK"
    )
  )
ergm::mcmc.diagnostics(model_2)
model_2_gof <- ergm::gof(model_2)
snafun::stat_plot_gof(model_2_gof)
model_2_gof

models <- list(base_model, model_1, model_2)
texreg::screenreg(models)
# --------------------------------------------------------------------------- #
# Iteration 3 + MCMC Diagnostics + GOF
model_3_params <- bondora_net ~ edges + gwb1dsp(decay=0.05, fixed=TRUE) +
  gwb2dsp(decay=0.05, fixed=TRUE) + gwb1degree(decay=0.05, fixed=TRUE)
model_3 <- ergm::ergm(model_3_params, 
                      constraints= ~ bd(minout = 0, maxout = 20),
                      control = ergm::control.ergm(
                        MCMC.burnin = 10000,
                        MCMC.samplesize = 50000,
                        seed = 42,
                        MCMLE.maxit = 10,
                        parallel = 10,
                        parallel.type = "PSOCK"
                      )
  )
ergm::mcmc.diagnostics(model_3)
model_3_gof <- ergm::gof(model_3)
snafun::stat_plot_gof(model_3_gof)
model_3_gof

models <- list(base_model, model_1, model_2, model_3)
texreg::screenreg(models)
# --------------------------------------------------------------------------- #
