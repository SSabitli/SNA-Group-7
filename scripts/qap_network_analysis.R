# --------------------------------------------------------------------------- #
# NOTE: NOT THE LATEST QAP ANALYSIS!

# If not already Installed
install.packages("viridis") # For Colours
install.packages("here")    # To locate files from RProj

# Set colour palette
cols <- viridis::viridis(30)

# Ensure repeatability
set.seed(42)
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
occup_mat <- readRDS(paste0(qap_path,"adj_mat_occup.Rds"))
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

# Make function to run QAPs automatically
run_QAP <- function(y_mat, xlist, varnames, model_name) {
  
  # Run QAP Linear Regression
  model <- sna::netlm(y = y_mat, x = xlist, nullhyp = "qapspp", reps = 1000)
  model$names <- varnames
  summary(model)
  
  # Obtain results from Model and Name them
  results <- model$coefficients
  names(results) <- varnames
  
  # Add significance stars to model results
  results_sig <- paste(round(results,3), t_to_stars(model$tstat))
  
  # Save Model
  saveRDS(model, here::here("resources","objects","qap",
                            paste0(model_name,".Rds")))
  
  # Prepare function output
  list_names <- c("model","results","results_sig")
  items <- list(model, results, results_sig)
  names(items) <- list_names
  
  return(items)
}

var_names <- c("Intercept", "Rating", "Occupation","Loan Amount", "Age",
               "Gender", "Loan Duration", "Restructured")
main_pred_names <- c("Intercept", "Rating", "Occupation")
main_pred_vars <- list(rating_mat, occup_mat)
all_pred_vars <- list(rating_mat, occup_mat, amt_diffs_mat, age_diffs_mat,
                  gender_mat, loandur_diffs_mat, rest_mat)
# --------------------------------------------------------------------------- #
# Basic QAP Linear Regression 1 - Unstandardised + No Controls

qap_m1 <- run_QAP(loan_use_mat, main_pred_vars, main_pred_names, "qap_m1")
summary(qap_m1$model)

# --------------------------------------------------------------------------- #
# Basic QAP Linear Regression 1 - Standardised + No Controls
scaled_dep <- scale(loan_use_mat)
scaled_pred <- lapply(main_pred_vars, scale)

qap_m2 <- run_QAP(scaled_dep, scaled_pred, main_pred_names, "qap_m2")
summary(qap_m2$model)

# --------------------------------------------------------------------------- #
# Plot the result for Model 1 Unstandardised vs Standardised

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
# --------------------------------------------------------------------------- #
# Basic QAP Linear Regression 2 - Unstandardised + Controls

qap_m3 <- run_QAP(loan_use_mat, all_pred_vars, var_names, "qap_m3")
summary(qap_m3$model)

# --------------------------------------------------------------------------- #
# Basic QAP Linear Regression 2 - Standardised + Controls
scaled_pred_2 <- lapply(all_pred_vars, scale)

qap_m4 <- run_QAP(scaled_dep, scaled_pred_2, var_names, "qap_m4")
summary(qap_m4$model)

# --------------------------------------------------------------------------- #
