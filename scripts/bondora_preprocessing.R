# --------------------------------------------------------------------------- #
install.packages("here") # To locate files within directory easier
install.packages("viridis") # For Colours
# --------------------------------------------------------------------------- #
# Import the Bondora P2P Dataset
bondora_raw <- read.csv(here::here(
  "dataset","LoanData_Bondora.csv"))
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

# Make function to consistently plot comparisons
plot_desc_hists <- function(df1, df2, col_name, type) {
  
  par(mfrow=c(1,3))
  
  hist(df1[[col_name]], xlab=type, col=cols[15], main="", breaks=10)
  mtext("Full Sample", side=3, adj=0, line=1, cex=1, font=2)

  hist(df2[[col_name]], xlab=type, col=cols[15], main="", breaks=10)
  mtext("Processed Sample", side=3, adj=0, line=1, cex=1, font=2) 

  qqplot(df1[[col_name]], df2[[col_name]], main="", cex=1,
         xlab="Full Sample", ylab="Subsample")
  abline(0, 1, lty=2)
  
  mtext("QQ Plot", side=3, adj=0, line=1, cex=1, font=2)                          
  
  # Reset plot window
  par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
}

plot_desc_bar <- function(df1, df2, col_name, type) {
  
  par(mfrow=c(1,2), mar=c(5,10,4,2))
  
  barplot(sort(table(df1[[col_name]]), decreasing = F),
          xlab=type, col=cols[15], horiz=TRUE, las=1)
  mtext("Full Sample", side=3, adj=0, line=1, cex=1, font=2)
  
  barplot(sort(table(df2[[col_name]]), decreasing = F),
          xlab=type, col=cols[15], horiz=TRUE, las=1)
  mtext("Processed Sample", side=3, adj=0, line=1, cex=1, font=2)
  
  # Reset plot window
  par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
}

plot_desc_hists(bondora, bondora_clean, "Amount", "Amount")
plot_desc_hists(bondora, bondora_clean, "Interest", "Interest")
plot_desc_hists(bondora, bondora_clean, "LoanDuration", "Loan Duration")
plot_desc_hists(bondora, bondora_clean, "MonthlyPayment", "Monthly Payment")
plot_desc_hists(bondora, bondora_clean, "Age", "Age")

plot_desc_bar(bondora, bondora_clean,"UseOfLoan_factor","Loan Purpose")
# --------------------------------------------------------------------------- #
# Convert Dataset into Incidence Matrix to form Network Object
bondora_slim <- bondora_clean

# Create the Incidence Matrix for Use of Loan
bondora_matrix <- table(
  bondora_slim$UserName, bondora_slim$UseOfLoan)
#bondora_matrix[bondora_matrix > 0] <- 1

# Create network object with counts as Edge attribute
bondora_net <- network::network(
  bondora_matrix, directed=FALSE, bipartite=TRUE, 
  ignore.eval = FALSE, names.eval="frequency")

# Set the bipartite Attribute
len <- dim(bondora_matrix)[1]
len_b2 <- dim(bondora_matrix)[2]
network::set.vertex.attribute(
  bondora_net, "bipartite", value = rep(len,len), v=1:len
)

# Extract Partition 2 Labels
loan_use <- levels(bondora_clean$UseOfLoan_factor)

# Create Loan Type Attribute for Partition 2
b2_loantype <- rep(NA, len)
b2_loantype <- c(b2_loantype, loan_use)
network::set.vertex.attribute(
  bondora_net, "b2_loantype", value = b2_loantype
)

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
saveRDS(bondora_net, file="resources/objects/bondora_net.RDS")
# --------------------------------------------------------------------------- #
