# --------------------------------------------------------------------------- #
# If not already Installed
install.packages("here")

# Import the Network and Other Object
p2p_network <- readRDS(here::here("resources","objects","p2p_network.RDS"))
bondora_df <- readRDS(here::here("resources","objects","bondora_sample.RDS"))

# --------------------------------------------------------------------------- #

# Summary Statistics
snafun::g_summary(p2p_network)
snafun::has_loops(p2p_network)

# Degree and Betweenness Distribution
deg_dist <- snafun::g_degree_distribution(p2p_network)
bet_dist <- snafun::v_betweenness(p2p_network)

par(mfrow = c(2, 2))

hist(deg_dist,
     main = "Unadjusted Degree Distribution",
     xlab = "Node Degree",
     col = "cornsilk2",
     border = "cornsilk4")

hist(log(deg_dist),
     main = "Logarithm of Degree Distribution",
     xlab = "Node Degree",
     col = "cornsilk2",
     border = "cornsilk4")

hist(bet_dist,
     main = "Unadjusted Betweenness",
     xlab = "Betweenness",
     col = "cornsilk2",
     border = "cornsilk4")

hist(log(bet_dist),
     main = "Logarithm of Betweenness",
     xlab = "Betweenness",
     col = "cornsilk2",
     border = "cornsilk4")

# --------------------------------------------------------------------------- #
# Quick and Dirty Removal of Loops
p2p_network_clean <- snafun::remove_loops(p2p_network)


# Quick Test for ERGM Convergence
p2p_network_sna <- snafun::to_network(p2p_network_clean)

barebone_ERGM <- ergm::ergm(p2p_network_sna ~ edges)
texreg::screenreg(barebone_ERGM)
barebone.gof <- ergm::gof(barebone_ERGM)
snafun::stat_plot_gof(barebone.gof)
barebone.gof

simple_ERGM_1 <- ergm::ergm(p2p_network_sna ~ edges 
                            + #gwesp(decay=0.5, fixed=TRUE),
                              kstar(2),
                            control = ergm::control.ergm(
                              MCMC.burnin = 10000,
                              MCMC.samplesize = 50000,
                              seed = 123456,
                              MCMLE.maxit = 10)
                              #parallel = 3,
                              #parallel.type = "PSOCK")
                            )
ergm::mcmc.diagnostics(simple_ERGM_1)

texreg::screenreg(list(barebone_ERGM, simple_ERGM_1))




