# --------------------------------------------------------------------------- #
install.packages("here") # To locate files within directory easier
# --------------------------------------------------------------------------- #
# Import the Bondora P2P Dataset
bondora_raw <- read.csv(here::here("dataset","LoanData_Bondora.csv"),
                        header = TRUE)
raw_cols <- colnames(bondora_raw)
# --------------------------------------------------------------------------- #
# Subset only Columns we need
keep_cols <- c("LoanId", "UserName","NewCreditCustomer","LanguageCode",
               "Age", "Gender", "Country", "Amount", "Interest",
               "LoanDuration","UseOfLoan", "Education", "MaritalStatus",
               "NrOfDependants", "Rating", "Restructured", 
               "NoOfPreviousLoansBeforeLoan", "MonthlyPayment")

keep_cols_slim <- c("LoanId", "UserName",
                    "Age", "Gender", "Country", "Amount", "Interest",
                    "LoanDuration","UseOfLoan", "Education", "MaritalStatus", 
                    "Rating", "Restructured", "NoOfPreviousLoansBeforeLoan", 
                    "MonthlyPayment")

bondora <- bondora_raw[keep_cols]

# Remove Rows with any NAs -> Complete Dataset Preferred
print(paste("NA Count |",sum(is.na(bondora)),"rows"))
bondora <- na.omit(bondora)
sum(is.na(bondora))

# Observe Class of Each Attribute
sapply(bondora, class)

# Make New Customer Indicator Binary
new_customer_mapping <- c("True" = 1, "False" = 0)
bondora$NewCreditCustomer <- new_customer_mapping[
  bondora$NewCreditCustomer]
# --------------------------------------------------------------------------- #
# Observe "Population" Descriptive Statistics
summary(bondora$Amount)
boxplot(bondora$Amount, main="Loan Amounts")

summary(bondora$Interest)
boxplot(bondora$Interest, main="Interest Rate (Maximum)")

summary(bondora$LoanDuration)
boxplot(bondora$LoanDuration, main="Loan Duration in Months")

summary(bondora$NoOfPreviousLoansBeforeLoan)
barplot(table(bondora$NoOfPreviousLoansBeforeLoan),
        main = "Number of Previous Loans")

summary(bondora$MonthlyPayment)
boxplot(bondora$MonthlyPayment, main="Monthly Payments")
# --------------------------------------------------------------------------- #
# Randomly Remove Observations until Desired Size is Reached
set.seed(42)
sample_size <- 500
sample_indices <- sample(1:nrow(bondora), sample_size)
bondora_sample <- bondora[sample_indices, ]
# --------------------------------------------------------------------------- #
# Test Sample Representativeness using Kolmogorov-Smirnov Test
numeric_cols <- c("Amount", "Interest","LoanDuration","MonthlyPayment")

for (col in numeric_cols) {
  print(paste("Test condcuted for",col))
  print(ks.test(bondora[[col]], bondora_sample[[col]]))
}



