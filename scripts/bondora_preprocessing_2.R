# --------------------------------------------------------------------------- #
install.packages("here") # To locate files within directory easier\
install.packages("RColorBrewer") # For Colours
# --------------------------------------------------------------------------- #
# Import the Bondora P2P Dataset
bondora_raw <- readxl::read_excel(here::here(
  "dataset","bondora_older_dataset.xlsx"))
raw_cols <- colnames(bondora_raw)
# --------------------------------------------------------------------------- #
# Select Columns to Keep
keep_cols <- c("LoanId", "UserName", "AuctionNumber","AuctionBidNumber",
              "Age", "Gender", "Country", "Amount", "Interest",
              "City","LoanDuration","UseOfLoan", "Education", "MaritalStatus", 
              "Rating", "Restructured", "MonthlyPayment",
              "LoanApplicationStartedDate")

bondora <- bondora_raw[keep_cols]

# Remove Rows with any NAs -> Complete Dataset Preferred
print(paste("NA Count |",sum(is.na(bondora)),"rows"))
bondora <- na.omit(bondora)
print(paste("NA Count |",sum(is.na(bondora)),"rows"))

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
# Randomly Remove Observations until Desired Size is Reached
set.seed(42)
sample_size <- 500
sample_indices <- sample(1:nrow(bondora), sample_size)
bondora_sample <- bondora[sample_indices, ]
# --------------------------------------------------------------------------- #
# Test Sample Distributions using Kolmogorov-Smirnov Test
numeric_cols <- c("Amount", "Interest","LoanDuration","MonthlyPayment")

for (col in numeric_cols) {
  print(paste("Test condcuted for",col))
  print(ks.test(bondora[[col]], bondora_sample[[col]]))
}
# --------------------------------------------------------------------------- #
# Convert Dataset into Edge List to form Network Object
bondora_slim <- bondora[c("UserName","AuctionNumber")]

# Initialise Variables for the Empty Data Structures
len <- nrow(bondora_slim)
bondora_edgelist <- data.frame(from = character(), to = character())

# Loop to Associate Users with Auctions they Participated In
for (i in 1:len) {
  for (j in (i+1):len) {
    
    if (j>len) {
      break
    }
    
    participation <- bondora_slim[i,2] == bondora_slim[j,2]
    
    if (participation) {
      temp_user1 <- bondora_slim[i,1]
      temp_user2 <- bondora_slim[j,1]
      temp <- data.frame("from"=temp_user1, "to"=temp_user2)
      bondora_edgelist <- rbind(bondora_edgelist, temp)
    }
  }
}

# Remove Loops, if any
sum(duplicated(bondora_edgelist))
bondora_edgelist <- bondora_edgelist[!duplicated(bondora_edgelist),]


