# --------------------------------------------------------------------------- #
# If not already Installed
install.packages("viridis") # For Colours
#install.packages("here")   # To locate files from RProj

# Set colour palette
cols <- viridis::viridis(30)
# --------------------------------------------------------------------------- #
# Load Relevant Files
qap_path="resources/objects/qap/"
loan_use_mat <- readRDS(paste0(qap_path,"adj_mat_loanuse.RDS"))
rating_mat <- readRDS(paste0(qap_path,"adj_mat_rating.RDS"))
amt_diffs_mat <- readRDS(paste0(qap_path,"adj_mat_amtdiffs.RDS"))
age_diffs_mat <- readRDS(paste0(qap_path,"adj_mat_agediffs.RDS"))
gender_mat <- readRDS(paste0(qap_path,"adj_mat_gender.RDS"))
loandur_diffs_mat <- readRDS(paste0(qap_path,"adj_mat_loandurdiffs.RDS"))
rest_mat <- readRDS(paste0(qap_path,"adj_mat_rest.RDS"))
# --------------------------------------------------------------------------- #
# Create function to determine significance from t-value statistic
t_to_stars <- function(t) {
  stars <- rep("", length(t))
  stars[abs(t) >= 1.96]  <- "*"
  stars[abs(t) >= 2.576] <- "**"
  stars[abs(t) >= 3.291] <- "***"
  
  return(stars)
}

# Make function to save plots
save_qap_plot <- function(plt_nam) {
  plt <- recordPlot()
  saveRDS(plt, here::here("resources","objects","qap",
                          paste0(plt_nam,".Rds")))
}

var_names <- c("Intercept", "Rating","Loan Amount","Age","Gender",
               "Loan Duration","Restructured")
pred_vars <- list(rating_mat, amt_diffs_mat, age_diffs_mat,
                  gender_mat, loandur_diffs_mat, rest_mat)
# --------------------------------------------------------------------------- #
# Basic QAP Linear Regression
qap_m1 <- sna::netlm(y = loan_use_mat,
                     x = list(rating_mat, amt_diffs_mat, age_diffs_mat,
                              gender_mat, loandur_diffs_mat, rest_mat),
                     nullhyp = "qapspp", reps = 2500)
qap_m1$names <- var_names
summary(qap_m1)

results_m1 <- qap_m1$coefficients
names(results_m1) <- var_names
results_sig_m1 <- paste(round(results_m1,3), t_to_stars(qap_m1$tstat))

# Plot Residuals
hist(qap_m1$residuals, main="QAP Residuals", col = cols[15])

# Save Model
saveRDS(qap_m1, file = paste0(qap_path,"qap_m1.RDS"))
# --------------------------------------------------------------------------- #
# Standardised QAP Linear Regression
scaled_dep <- scale(loan_use_mat)
scaled_pred <- lapply(pred_vars, scale)

qap_m2 <- sna::netlm(y = scaled_dep,
                     x = scaled_pred,
                     nullhyp = "qapspp", reps = 2500)
qap_m2$names <- var_names
summary(qap_m2)

# Plot the result
results_m2 <- qap_m2$coefficients
names(results_m2) <- var_names
results_sig_m2 <- paste(round(results_m2,3), t_to_stars(qap_m2$tstat))

# Plot Residuals
hist(qap_m2$residuals, main="QAP Residuals", col = cols[15])

# Save Model
saveRDS(qap_m2, file = paste0(qap_path,"qap_m2.RDS"))
# --------------------------------------------------------------------------- #
# Plot the result
par(mfrow=c(1,2))

qap_plot_m1 <- barplot(results_m1, col = cols[15],  border = cols[10], 
                       ylim = c(min(results_m1) + min(results_m1)*0.15, 
                                max(results_m1) + max(results_m1)*0.15),
                       main="QAP Model Results (Unstandardised)")
text(x = qap_plot_m1, 
     y = results_m1 + sign(results_m1)*(0.075*diff(range(results_m1))), 
     labels = results_sig_m1, font = 2)


qap_plot_m2 <- barplot(results_m2, col = cols[15],  border = cols[10], 
                       ylim = c(min(results_m2) + min(results_m2)*0.15, 
                                max(results_m2) + max(results_m2)*0.15),
                       main="QAP Model Results (Standardised)")
text(x = qap_plot_m2, 
     y = results_m2 + sign(results_m2)*(0.075*diff(range(results_m2))), 
     labels = results_sig_m2, font = 2)

save_qap_plot("unstd_std_plot")

par(mfrow=c(1,1))
