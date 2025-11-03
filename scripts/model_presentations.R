# --------------------------------------------------------------------------- #
# install.packages("here")
path <- 'resources/objects/'
# --------------------------------------------------------------------------- #
# QAPs

# --------------------------------------------------------------------------- #
# ERGM MODEL 1 | BASE MODEL
ergm_1 <- readRDS(paste0(path,"ergm/","ergm_base_panel",".RDS"))

# View Model Summary
texreg::screenreg(list(ergm_1$model))

# View GOF
ergm_1$gof
snafun::stat_plot_gof(ergm_1$gof)
# --------------------------------------------------------------------------- #
