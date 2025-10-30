# --------------------------------------------------------------------------- #
install.packages("here") # To locate files within directory easier\
install.packages("RColorBrewer") # For Colours
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
# --------------------------------------------------------------------------- #
# Observe Descriptive Statistics for the Full Sample

cols <- RColorBrewer::brewer.pal(n = 3, name = "RdBu")

# Amounts
par(mfrow=c(1,2))
summary(bondora$Amount)
hist(bondora$Amount, xlab="Amount", col=cols[1], main="")
boxplot(bondora$Amount, ylab="Amount",col=cols[1], main="")

# Interest Rates
par(mfrow=c(1,2))
summary(bondora$Interest)
hist(bondora$Interest, xlab="Interest Rate (%)", col=cols[1], main="")
boxplot(bondora$Interest, ylab="Interest Rate (%)",col=cols[1], main="")

# Loan Duration
par(mfrow=c(1,2))
summary(bondora$LoanDuration)
hist(bondora$LoanDuration, xlab="Loan Duration (Months)", col=cols[1], main="")
boxplot(bondora$LoanDuration,ylab="Loan Duration (Months)",col=cols[1], main="")

# Monthly Payment
par(mfrow=c(1,2))
summary(bondora$MonthlyPayment)
hist(bondora$MonthlyPayment, xlab="Monthly Payment", col=cols[1], main="")
boxplot(bondora$MonthlyPayment,ylab="Monthly Payment",col=cols[1], main="")

# Monthly Payment
par(mfrow=c(1,2))
summary(bondora$MonthlyPayment)
hist(bondora$MonthlyPayment, xlab="Monthly Payment", col=cols[1], main="")
boxplot(bondora$MonthlyPayment,ylab="Monthly Payment",col=cols[1], main="")
# --------------------------------------------------------------------------- #
# Test Sample Distributions using Kolmogorov-Smirnov Test
numeric_cols <- c("Amount", "Interest","LoanDuration","MonthlyPayment")

for (col in numeric_cols) {
  print(paste("Test condcuted for",col))
  print(ks.test(bondora[[col]], bondora_sample[[col]]))
}
# --------------------------------------------------------------------------- #
# Convert Dataset into Incidence Matrix to form Network Object
bondora_slim <- bondora_clean

# Create the Incidence Matrix for Use of Loan
bondora_matrix <- table(
  bondora_slim$UserName, bondora_slim$UseOfLoan)
bondora_matrix[bondora_matrix > 0] <- 1

# See distribution of UseofLoan
barplot(table(bondora_slim$UseOfLoan))

# Create network object
bondora_net <- network::network(
  bondora_matrix, directed=FALSE, bipartite=TRUE)

# Extract Vectors for Vertex Attributes
age <- bondora_clean$Age[match(
  rownames(bondora_matrix), bondora_clean$UserName)]

# Set the bipartite Attribute
len <- dim(bondora_matrix)[1]
network::set.vertex.attribute(
  bondora_net, "bipartite", value = rep(len,len), v=1:len
)

# Save the network object
saveRDS(bondora_net, file="resources/objects/bondora_net.RDS")
# --------------------------------------------------------------------------- #
