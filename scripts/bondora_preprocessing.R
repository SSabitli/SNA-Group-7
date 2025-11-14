# --------------------------------------------------------------------------- #
install.packages("viridis") # For Colours
install.packages("here") # To locate files from RProj
set.seed(42)
# --------------------------------------------------------------------------- #
# Import the Bondora P2P Dataset
obj_paths = "resources/objects/"
bondora_raw <- read.csv("dataset/LoanData_Bondora.csv")
raw_cols <- colnames(bondora_raw)
# --------------------------------------------------------------------------- #
# Select Columns to Keep
keep_cols <- c("LoanId", "UserName","Age", "Gender", 
               "Country", "Amount", "Interest","LoanDuration",
              "UseOfLoan", "Rating", "Restructured", "MonthlyPayment",
              "OccupationArea", "BiddingStartedOn")

bondora <- bondora_raw[keep_cols]

# Change date format to the correct one
bondora$BiddingStartedOn <- as.POSIXct(bondora$BiddingStartedOn, 
                                       format = "%Y-%m-%d %H:%M:%S")
bondora$year <- as.numeric(format(bondora$BiddingStartedOn, "%Y"))

# Remove Rows with any NAs -> Complete Dataset Preferred
print(paste("NA Count |",sum(is.na(bondora)),"rows"))
bondora <- na.omit(bondora)
print(paste("NA Count |",sum(is.na(bondora)),"rows"))

# Observe the distribution of Loan Use over Time
unique_counts <- tapply(bondora$UserName,
                        list(bondora$year, bondora$UseOfLoan),
                        function(x) length(unique(x)))
unique_counts[is.na(unique_counts)] <- 0

barplot(t(unique_counts),           # transpose so bars = loan types
        beside = TRUE,              # side-by-side bars
        col = viridis::viridis(ncol(unique_counts)),
        legend.text = colnames(unique_counts),
        args.legend = list(x = "topright", cex = 0.8),
        xlab = "Year",
        ylab = "Number of Unique Users")
user_counts_plt <- recordPlot()
saveRDS(user_counts_plt,
        here::here("resources","objects","preprocessing","user_cnt_plt.Rds"))

# Restrict to Recent Time Period
bondora_test <- bondora[bondora$year > 2013 & bondora$year < 2017, ]

# See the distribution of LoanUses
see_counts <- function(var1, var2) {
  counts <- table(var1, var2)
  total_counts <- colSums(counts)
  prop <- total_counts / sum(total_counts)
  
  return(list(total_counts, round(prop,2)))
}
before_counts <- see_counts(bondora_test$UserName, bondora_test$UseOfLoan)
barplot(before_counts[[1]])

# Number of unique users in the subsample
unique_users <- unique(bondora_test$UserName)
sample_users <- sample(unique_users, 500)
bondora_clean <- bondora_test[bondora_test$UserName %in% sample_users, ]

after_counts <- see_counts(bondora_clean$UserName, bondora_clean$UseOfLoan)
barplot(after_counts[[1]])

# DEPRECATED METHOD
# Filter People with 2+ Loans
#user_counts <- table(bondora_test$UserName)
#multi_users <- names(user_counts[user_counts > 2])
#bondora_clean <- bondora_test[bondora_test$UserName %in% multi_users, ]

# DEPRECATED METHOD
# Remove Users with only One Loan 
#user_counts <- table(bondora_clean$UserName)
#multi_users <- names(user_counts[user_counts > 5])
#bondora_clean <- bondora_clean[bondora_clean$UserName %in% multi_users, ]
#bondora_test <- bondora_raw[bondora_raw$UserName %in% multi_users, ]

# See if Ratings are Properly Encoded
unique(bondora_clean$Rating)

# Extract UseofLoan Types and Turn into Factor
bondora_clean$UseOfLoan_factor <- as.factor(bondora_clean$UseOfLoan)
unique(bondora_clean$UseOfLoan_factor)

loan_use_labels <- c(
  "-1" = "NA",
  "0" ="Loan Consolidation", 
  "1" = "Real Estate",
  "2" = "Home Improvement",
  "3" = "Business",
  "4" = "Education",
  "5" = "Travel",
  "6" = "Vehicle",
  "7" = "Other",
  "8" = "Health",
  "110" = "Other Business",
  "102" = "Undefined Business",
  "108" = "Undefined Business"
)
# Change Labels for Cleaned Dataset
bondora_clean$UseOfLoan_factor <- loan_use_labels[
  as.character(bondora_clean$UseOfLoan)]

# Change Labels for Uncleaned Dataset
bondora_test$UseOfLoan_factor <- loan_use_labels[
  as.character(bondora_test$UseOfLoan)]

# Add labels to the OccupationArea Variable
levels(as.factor(bondora_clean$OccupationArea)) # view codes

occupation_labels <- c(
  "-1" = "NA",
  "1" = "Other",
  "2" = "Mining",
  "3" = "Processing",
  "4" = "Energy",
  "5" = "Utilities",
  "6" = "Construction",
  "7" = "Retail",
  "8" = "Transport",
  "9" = "Hospitality",
  "10" = "Info/Telcom",
  "11" = "Finance",
  "12" = "Realestate",
  "13" = "Research",
  "14" = "Admin",
  "15" = "Civil/Mil",
  "16" = "Education",
  "17" = "Healthcare",
  "18" = "Social",
  "19" = "Art/Ent",
  "20" = "Agriculture",
  "21" = "Forestry/Fish"
)
# store original
bondora_clean$occupation_code <- bondora_clean$OccupationArea
bondora_clean$occupation_label <- occupation_labels[
  as.character(bondora_clean$OccupationArea)]

bondora_test$occupation_code <- bondora_test$OccupationArea
bondora_test$occupation_label <- occupation_labels[
  as.character(bondora_test$OccupationArea)]
# --------------------------------------------------------------------------- #
# Observe Descriptive Statistics 

cols <- viridis::viridis(30)

# Make function to save plots
save_plot <- function(plt_nam) {
  plt <- recordPlot()
  saveRDS(plt, here::here("resources","objects","preprocessing",
                     paste0(plt_nam,".Rds")))
}

# Make function to consistently plot comparisons
plot_desc_hists <- function(df1, df2, col_name, type) {
  
  par(mfrow=c(1,3))
  
  hist(df1[[col_name]], xlab=type, col=cols[15], main="", breaks=10)
  mtext("Initial Sample", side=3, adj=0, line=0.25, cex=1, font=2)

  hist(df2[[col_name]], xlab=type, col=cols[15], main="", breaks=10)
  mtext("Reduced Sample", side=3, adj=0, line=0.25, cex=1, font=2) 

  qqplot(df1[[col_name]], df2[[col_name]], main="", cex=1,
         xlab="Full Sample", ylab="Subsample", line=0.25)
  abline(0, 1, lty=2)
  
  mtext("QQ Plot", side=3, adj=0, line=0.25, cex=1, font=2)  
  
  mtext(type, outer = TRUE, line = -2, side=3, cex = 1.3, font = 2)
  
  # Reset plot window
  par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
}

plot_desc_bar <- function(df1, df2, col_name, type) {
  
  par(mfrow=c(1,2), mar=c(5,10,4,2))
  
  barplot(sort(table(df1[[col_name]]), decreasing = F),
          xlab=type, col=cols[15], horiz=TRUE, las=1)
  mtext("Initial Sample", side=3, adj=0, line=0.25, cex=1, font=2)
  
  barplot(sort(table(df2[[col_name]]), decreasing = F),
          xlab=type, col=cols[15], horiz=TRUE, las=1)
  mtext("Reduced Sample", side=3, adj=0, line=0.25, cex=1, font=2)
  
  mtext(type, outer = TRUE, line = -2, side=3, cex = 1.3, font = 2)
  
  # Reset plot window
  par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
}

plot_desc_hists(bondora_test, bondora_clean, "Amount","Amount")
save_plot("hist_amt")

plot_desc_hists(bondora_test, bondora_clean, "Interest","Interest")
save_plot("hist_int")

plot_desc_hists(bondora_test, bondora_clean, "LoanDuration","Loan Duration")
save_plot("hist_loandur")

plot_desc_hists(bondora_test, bondora_clean, "MonthlyPayment","Monthly Payment")
save_plot("hist_monpmt")

plot_desc_hists(bondora_test, bondora_clean, "Age","Age")
save_plot("hist_age")

plot_desc_bar(bondora_test, bondora_clean,"UseOfLoan_factor","Loan Purpose")
save_plot("bar_loanuse")

plot_desc_bar(bondora_test, bondora_clean,"Rating","Credit Rating")
save_plot("bar_rating")

plot_desc_bar(bondora_test, bondora_clean,"occupation_label","Occupation")
save_plot("bar_occupation")

# Get Tabular Summary Statistics
tab_comps <- function(df1, df2, cols) {
  stats <- c("Mean"=mean, "Median"=median, "Std. Dev."=sd, "Min"=min, "Max"=max)
  
  get_stats <- function(d) {
    t(sapply(d[cols], function(x)
      sapply(stats, function(f) round(f(x, na.rm=TRUE), 2))
    ))
  }
  
  df1_stats <- get_stats(df1)
  df2_stats <- get_stats(df2)
  
  out <- rbind(
    cbind(DataFrame = "Initial Sample", 
          Variable = rownames(df1_stats), df1_stats),
    cbind(DataFrame = "Reduced Sample", 
          Variable = rownames(df2_stats), df2_stats)
  )
  rownames(out) <- NULL
  as.data.frame(out)
}

tab_results <- tab_comps(bondora_test, bondora_clean,
                         c("Amount","Interest","Age","LoanDuration"))

knitr::kable(tab_results)

# Save table for use in the Report
saveRDS(tab_results, 
        file=paste0(obj_paths,"preprocessing/","summary_table.Rds"))
# --------------------------------------------------------------------------- #
# Convert Dataset into Incidence Matrix to form Network Object (for ERGM)
bondora_slim <- bondora_clean

# Create the Incidence Matrix for Use of Loan
bondora_matrix <- table(
  bondora_slim$UserName, bondora_slim$UseOfLoan)
bondora_matrix[bondora_matrix > 0] <- 1 # Given that ergm.counts fails with GOF

# Create network object with counts as Edge attribute
bondora_net <- network::network(
  bondora_matrix, directed=FALSE, bipartite=nrow(bondora_matrix), 
  ignore.eval = FALSE, names.eval="frequency", loops=FALSE)

# Set the bipartite Attribute - UNNCESSARY GIVEN bipartite=length(n)
len <- dim(bondora_matrix)[1]
len_b2 <- dim(bondora_matrix)[2]
b_indicator <- c(rep(1,len),rep(2,len_b2))

# Extract Partition 2 Labels
loan_use <- levels(bondora_clean$UseOfLoan_factor)

# Create Loan Type Attribute for Partition 2
b2_loantype <- rep(NA, len)
b2_loantype <- c(b2_loantype, loan_use)

if (length(b2_loantype) == network::network.size(bondora_net)) {
  network::set.vertex.attribute(
    bondora_net, "b2_loantype", value = b2_loantype)
  }

# Add Age Vertex Attribute to B1
age <- bondora_clean$Age[match(
  rownames(bondora_matrix), bondora_clean$UserName)]
b1_age <- c(age, rep(NA, len_b2))
network::set.vertex.attribute(
  bondora_net, "b1_age", value=b1_age
)

# Add Gender Vertex Attribute to B1
gender <- bondora_clean$Gender[match(
  rownames(bondora_matrix), bondora_clean$UserName)]
unique(gender) # Check to see if encoded properly
gender <- ifelse(gender == 0, "male","female")
b1_gender <- c(gender, rep(NA, len_b2))
network::set.vertex.attribute(
  bondora_net, "b1_gender", value = b1_gender
)

# Save the network and data frame object
saveRDS(bondora_slim, file=paste0(obj_paths,"preprocessing/","bondora_df.Rds"))
saveRDS(bondora_net, file=paste0(obj_paths,"preprocessing/","bondora_net.Rds"))
# --------------------------------------------------------------------------- #
# First, create Incidence Matrix again but with Frequency Counts
loan_use_matrix <- table(
  bondora_slim$UserName, bondora_slim$UseOfLoan)

# Get the Adjacency Matrix for Loan Use Similarity (Dependent QAP Variable)
adj_mat_loan_use <- loan_use_matrix %*% t(loan_use_matrix)
# Remove self weights to remove any loops
diag(adj_mat_loan_use) <- 0

# Get the Adjacency Matrix for Credit Rating Similarity (Predictor in QAP)
incidence_rating <- table(bondora_slim$UserName, bondora_slim$Rating)
adj_mat_rating <- incidence_rating %*% t(incidence_rating)
diag(adj_mat_rating) <- 0

# Get Adjacency Matrix for Occupation Similarity (Predictor in QAP, Binary)
incidence_occupation <- table(bondora_slim$UserName, 
                              bondora_slim$occupation_label)
adj_mat_occupation <- incidence_occupation %*% t(incidence_occupation)
adj_mat_occupation[adj_mat_occupation > 0] <- 1
diag(adj_mat_occupation) <- 0

# Get the Adjacency Matrix for Loan Amount (Control in QAP) - BINARY
# First, bin the Loan Amounts
bondora_slim$Amount_bins <- cut(
  bondora_slim$Amount, breaks=c(0,2000,4000,6000,8000,10000),
  labels = c(1:5)
)
incidence_amount_bins <- table(bondora_slim$UserName, bondora_slim$Amount_bins)
adj_mat_amount_bins <- incidence_amount_bins %*% t(incidence_amount_bins)
diag(adj_mat_amount_bins) <- 0

# Continous Absolute Difference Approach
avg_amount <- tapply(bondora_slim$Amount, bondora_slim$UserName, mean)
adj_mat_amount_diff <- outer(avg_amount, avg_amount, 
                             FUN = function(x,y) abs(x - y))
diag(adj_mat_amount_diff) <- 0

# Get Matrix for Differences in Age
incidence_age <- table(bondora_slim$UserName, bondora_slim$Age)
borrower_ages <- as.numeric(colnames(incidence_age)[max.col(incidence_age)])
names(borrower_ages) <- rownames(incidence_age)
adj_mat_age <- outer(borrower_ages, borrower_ages, 
                    FUN = function(x, y) abs(x - y))
rownames(adj_mat_age) <- colnames(adj_mat_age) <- names(borrower_ages)

# Get Adjacency Matrix for (same) Gender
incidence_gender <- table(bondora_slim$UserName, bondora_slim$Gender)
adj_mat_gender <- incidence_gender %*% t(incidence_gender)
# Make the matrix binary for homophily
adj_mat_gender <- ifelse(adj_mat_gender > 0, 1, 0)
diag(adj_mat_gender) <- 0

# Get Adjacency Matrix for Differences in Average Loan Duration
borrower_loandur <- sapply(tapply(bondora_slim$LoanDuration, 
                                  bondora_slim$UserName, unique),
                           mean)
adj_mat_loandur_diff <- outer(borrower_loandur, borrower_loandur, 
                              FUN=function(x,y) abs(x-y))
diag(adj_mat_loandur_diff) <- 0

# Get Adjacency Matrix for Homophily in Restructure of Loans
incidence_restructure <- table(bondora_slim$UserName, bondora_slim$Restructured)
adj_mat_rest <- incidence_restructure %*% t(incidence_restructure)
diag(adj_mat_rest) <- 0

# Save the objects for the QAP Regression in different Script
qap_paths = paste0(obj_paths,"/qap/")
saveRDS(b_indicator, file=paste0(
  "resources/objects/preprocessing/indicator.Rds"))

saveRDS(adj_mat_loan_use, file=paste0(qap_paths,"adj_mat_loanuse.Rds"))
saveRDS(adj_mat_occupation, file=paste0(qap_paths,"adj_mat_occup.Rds"))
saveRDS(adj_mat_rating, file=paste0(qap_paths,"adj_mat_rating.Rds"))
saveRDS(adj_mat_amount_diff, file=paste0(qap_paths,"adj_mat_amtdiffs.Rds"))
saveRDS(adj_mat_age, file=paste0(qap_paths,"adj_mat_agediffs.Rds"))
saveRDS(adj_mat_gender, file=paste0(qap_paths,"adj_mat_gender.Rds"))
saveRDS(adj_mat_loandur_diff, file=paste0(qap_paths,"adj_mat_loandurdiffs.Rds"))
saveRDS(adj_mat_rest, file=paste0(qap_paths,"adj_mat_rest.Rds"))
# --------------------------------------------------------------------------- #


