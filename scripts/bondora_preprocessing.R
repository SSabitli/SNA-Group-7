# --------------------------------------------------------------------------- #
#install.packages("viridis") # For Colours
#install.packages("here") # To locate files from RProj
# --------------------------------------------------------------------------- #
# Import the Bondora P2P Dataset
obj_paths = "resources/objects/"
bondora_raw <- read.csv("dataset/LoanData_Bondora.csv")
raw_cols <- colnames(bondora_raw)
# --------------------------------------------------------------------------- #
# Select Columns to Keep
keep_cols <- c("LoanId", "UserName","Age", "Gender", 
               "Country", "Amount", "Interest","LoanDuration",
              "UseOfLoan", "Education", "MaritalStatus", 
              "Rating", "Restructured", "MonthlyPayment")

bondora <- bondora_raw[keep_cols]

# Remove Rows with any NAs -> Complete Dataset Preferred
print(paste("NA Count |",sum(is.na(bondora)),"rows"))
bondora <- na.omit(bondora)
print(paste("NA Count |",sum(is.na(bondora)),"rows"))

# Remove users with only -1 Stated UseofLoan
bondora_clean <- bondora[bondora$UseOfLoan != -1, ]

# Remove Users with only One Loan
user_counts <- table(bondora_clean$UserName)
multi_users <- names(user_counts[user_counts > 5])
bondora_clean <- bondora_clean[bondora_clean$UserName %in% multi_users, ]
#bondora_test <- bondora_raw[bondora_raw$UserName %in% multi_users, ]

# See if Ratings are Properly Encoded
unique(bondora_clean$Rating)

# See distribution of UserName Counts
hist(table(bondora_clean$UserName))
barplot(table(bondora_clean$UseOfLoan))

# Extract UseofLoan Types and Turn into Factor
bondora_clean$UseOfLoan_factor <- as.factor(bondora_clean$UseOfLoan)
unique(bondora_clean$UseOfLoan_factor)
levels(bondora_clean$UseOfLoan_factor) <- c(
  "Loan Consolidation", "Real Estate","Home Improvement",
  "Business","Education","Travel","Vehicle","Other","Health")

bondora$UseOfLoan_factor <- as.factor(bondora$UseOfLoan)
unique(bondora$UseOfLoan_factor)
levels(bondora$UseOfLoan_factor) <- c(
  "Unknown", "Loan Consolidation", "Real Estate","Home Improvement",
  "Business","Education","Travel","Vehicle","Other","Health", 
  "Machinery Purchase", "Acquisition Real Estate","Other Business"
)
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
  mtext("Full Sample", side=3, adj=0, line=0.25, cex=1, font=2)

  hist(df2[[col_name]], xlab=type, col=cols[15], main="", breaks=10)
  mtext("Processed Sample", side=3, adj=0, line=0.25, cex=1, font=2) 

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
  mtext("Full Sample", side=3, adj=0, line=0.25, cex=1, font=2)
  
  barplot(sort(table(df2[[col_name]]), decreasing = F),
          xlab=type, col=cols[15], horiz=TRUE, las=1)
  mtext("Processed Sample", side=3, adj=0, line=0.25, cex=1, font=2)
  
  mtext(type, outer = TRUE, line = -2, side=3, cex = 1.3, font = 2)
  
  # Reset plot window
  par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
}

plot_desc_hists(bondora, bondora_clean, "Amount", "Amount")
save_plot("hist_amt")

plot_desc_hists(bondora, bondora_clean, "Interest", "Interest")
save_plot("hist_int")

plot_desc_hists(bondora, bondora_clean, "LoanDuration", "Loan Duration")
save_plot("hist_loandur")

plot_desc_hists(bondora, bondora_clean, "MonthlyPayment", "Monthly Payment")
save_plot("hist_monpmt")

plot_desc_hists(bondora, bondora_clean, "Age", "Age")
save_plot("hist_age")

plot_desc_bar(bondora, bondora_clean,"UseOfLoan_factor","Loan Purpose")
save_plot("bar_loanuse")

plot_desc_bar(bondora, bondora_clean,"Rating","Credit Rating")
save_plot("bar_rating")
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
#network::set.vertex.attribute(
#  bondora_net, "bipartite", value = rep(len,len), v=1:len
#)

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

# Save the network object
saveRDS(bondora_net, file=paste0(obj_paths,"preprocessing/","bondora_net.Rds"))
# --------------------------------------------------------------------------- #
# Get the Adjacency Matrix for Loan Use Similarity (Dependent QAP Variable)
adj_mat_loan_use <- bondora_matrix %*% t(bondora_matrix)
# Remove self weights
diag(adj_mat_loan_use) <- 0

# Get the Adjacency Matrix for Credit Rating Similarity (Predictor in QAP)
incidence_rating <- table(bondora_slim$UserName, bondora_slim$Rating)
adj_mat_rating <- incidence_rating %*% t(incidence_rating)
diag(adj_mat_rating) <- 0

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
saveRDS(adj_mat_rating, file=paste0(qap_paths,"adj_mat_rating.Rds"))
saveRDS(adj_mat_amount_diff, file=paste0(qap_paths,"adj_mat_amtdiffs.Rds"))
saveRDS(adj_mat_age, file=paste0(qap_paths,"adj_mat_agediffs.Rds"))
saveRDS(adj_mat_gender, file=paste0(qap_paths,"adj_mat_gender.Rds"))
saveRDS(adj_mat_loandur_diff, file=paste0(qap_paths,"adj_mat_loandurdiffs.Rds"))
saveRDS(adj_mat_rest, file=paste0(qap_paths,"adj_mat_rest.Rds"))
# --------------------------------------------------------------------------- #


