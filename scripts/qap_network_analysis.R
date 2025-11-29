# --------------------------------------------------------------------------- #
# NOTE: NOT THE LATEST QAP ANALYSIS!

# If not already Installed
install.packages("viridis")   # For Colours
install.packages("here")      # To locate files from RProj
install.packages("gridExtra") # For multiple ggplot2 plots

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
  model <- sna::netlm(y = y_mat, x = xlist, nullhyp = "qapspp", reps = 3000)
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

# Make function to Plot QAP Coefficients
plot_coef_qap <- function(model,
                      model_name = "Model Estimates", 
                      sub = "Subtitle",
                      x_axis_label = "Coefficient Estimate") {
  
  # Extract Coefficients and SEs from ERGM 
  coefs <- model$coefficients
  ses <- model$coefficients / model$tstat
  
  # Make DF
  df <- data.frame(
    term = model$names,
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
      title = model_name,
      subtitle = sub
      #color = "Significance"
    )
}
# Make Function to Plot Diagnostics
# DEPRECATED SINCE PROGRESS MEETING 3
qap_diags <- function(model, title, filename) {
  resid <- model$model$residuals
  fitted <- model$model$fitted.values
  
  # Set viewing window to two plots
  par(mfrow=c(1,2))
  
  # Residuals Plot
  hist(resid, xlab="Residuals", 
       main=title)
  
  # Fitted vs Residuals
  plot(fitted, resid, xlab="Fitted Values", ylab="Residuals",
       main=title)
  
  # Capture and Save the Plot
  save_qap_plot(filename)
  
  # Reset plot view
  par(mfrow=c(1,1))
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

# Plot the QAP Model Coefficients and Save it
qap_m1_coef_plot <- plot_coef_qap(qap_m1$model,
                                  model_name = "MRQAP Model 1",
                                  sub = "No Controls")
saveRDS(qap_m1_coef_plot,
        here::here("resources","objects","qap","qap_m1_coef_plot.Rds"))

# Run Diagnostics Plots - DEPRECATED SINCE PROGRESS MEETING 3 SUGGESTIONS
#qap_diags(qap_m1,"QAP LR - Unstandardised + No Controls","qap_m1_diags")

# --------------------------------------------------------------------------- #
# Basic QAP Linear Regression 2 - Unstandardised + Controls
qap_m3 <- run_QAP(loan_use_mat, all_pred_vars, var_names, "qap_m3")

summary(qap_m3$model)

# Plot the QAP Model Coefficients and Save it
qap_m3_coef_plot <- plot_coef_qap(qap_m3$model,
                                  model_name = "MRQAP Model 2",
                                  sub = "With Controls")
saveRDS(qap_m3_coef_plot,
        here::here("resources","objects","qap","qap_m3_coef_plot.Rds"))

# Arrange the Plots together
qap_coef_plots <- gridExtra::grid.arrange(qap_m1_coef_plot, qap_m3_coef_plot,
                        ncol = 2)
# Save the Plots
saveRDS(qap_coef_plots,
        here::here("resources","objects","qap","qap_coef_plots.Rds"))

# Run Diagnostics Plots - DEPRECATED SINCE PROGRESS MEETING 3 SUGGESTIONS
#qap_diags(qap_m3,"QAP LR - Unstandardised + Controls","qap_m3_diags")

# --------------------------------------------------------------------------- #
# Collect Results in Table

# Extract R2 from the netlm object from the Console output
# It was too difficult to calculate R2 from scratch and netlm does not 
# provide this as an output directly from the netlm object
calc_r2 <- function(model) {
  
  out <- capture.output(model)
  r2_line <- grep("Multiple R-squared", out, value = TRUE)
  r2 <- as.numeric(
    sub(".*Multiple R-squared:\\s*([0-9\\.]+).*", "\\1", r2_line))
  
  return(r2)
}

# Make function to get details of netlm manually
extract.netlm <- function(model) {
  tr <- texreg::createTexreg(
    coef.names = model$names,
    coef = model$coefficients,
    se = model$coefficients / model$tstat,
    pvalues = model$pgreqabs,
    gof.names = c("R-squared"),
    gof = (calc_r2(model)),
    gof.decimal = c(TRUE)
  )
  return(tr)
}

# View the Table of Results
titles <- c("M1 | No Controls", "M2 | Controls")
extracted_models <- lapply(list(qap_m1, qap_m3), extract.netlm)
texreg::screenreg(extracted_models, custom.model.names = titles, digits = 3)

# Save the table for use in the Report
saveRDS(list(extracted_models, titles), 
        here::here("resources","objects","qap","qap_tables.Rds"))
# --------------------------------------------------------------------------- #
