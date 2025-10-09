
# --------------------------------------------------------------------------- #
# Reset WD
#setwd("../..")

# Import the Bondora P2P Dataset
bondora_raw <- read.csv("dataset/LoanData_Bondora.csv",
                        header = TRUE)
cols <- colnames(bondora_raw)

# --------------------------------------------------------------------------- #

# Subset only Columns we need
keep_cols <- c("LoanId", "UserName","NewCreditCustomer","LanguageCode",
               "Age", "Gender", "Country", "Amount", "Interest",
               "LoanDuration","UseOfLoan", "Education", "MaritalStatus",
               "NrOfDependants", "Rating", "Restructured", 
               "NoOfPreviousLoansBeforeLoan", "MonthlyPayment")
bondora <- bondora_raw[keep_cols]

# Remove Rows with NAs -> Complete Dataset Preferred
bondora_complete <- na.omit(bondora)
sum(is.na(bondora_complete))

# Observe Class of Each Attribute
sapply(bondora_complete, class)

# Make Binary Indicators Binary
new_customer_mapping <- c("True" = 1, "False" = 0)
bondora_complete$NewCreditCustomer <- new_customer_mapping[
  bondora_complete$NewCreditCustomer]

# Replace User inputs of Blank Dependants with Zero
bondora_complete$NrOfDependants[bondora_complete$NrOfDependants == ""] <- NA
bondora_complete$NrOfDependants[is.na(bondora_complete$NrOfDependants)] <- 0

# Make the Column Numeric
bondora_complete$NrOfDependants <- as.numeric(bondora_complete$NrOfDependants)
bondora_complete$NrOfDependants[is.na(bondora_complete$NrOfDependants)] <- 0

# Make Restructured Binary
bondora_complete$Restructured <- new_customer_mapping[
  bondora_complete$Restructured]

# Randomly Remove Observations until Desired Size is Reached
set.seed(42)
sample_size <- 500
sample_indices <- sample(1:nrow(bondora_complete), sample_size)
bondora_sample <- bondora_complete[sample_indices, ]

# --------------------------------------------------------------------------- #

# Choose Feature Subset for Similarity Metric
similarities <- c("LoanDuration","Amount","MonthlyPayment", "NewCreditCustomer",
                  "NoOfPreviousLoansBeforeLoan","LanguageCode")
bondora_similar <- bondora_sample[similarities]

# Standardise Numeric Features in the Similarity Set
bondora_similar_scaled <- scale(bondora_similar)

# Compute Cosine Similarity
cosine_sim <- function(X) {
  # numerator: dot product
  sim <- X %*% t(X)
  
  # denominator: product of norms
  norms <- sqrt(rowSums(X^2))
  sim <- sim / (norms %*% t(norms))
  
  return(sim)
}

similarity_matrix <- cosine_sim(bondora_similar_scaled)

# Get the Usernames for the Random Lenders
vertex_names <- as.character(bondora_sample$UserName)

# Get upper triangle indices
ut <- which(upper.tri(similarity_matrix), arr.ind = TRUE)

# Filter by threshold
ut <- ut[similarity_matrix[ut] >= threshold, ]

# Create edge list
p2p_bondera <- data.frame(
  from = vertex_names[ut[,1]],
  to   = vertex_names[ut[,2]],
  weight = similarity_matrix[ut],
  stringsAsFactors = FALSE
)

# Difference between attributes present and not present
att_diffs <- setdiff(keep_cols, similarities)

# Merge Data Frames to Ensure other Attributes Appear in Edge List
p2p_bondera <- merge(p2p_bondera, bondora_sample[att_diffs],
               by.x = "from", by.y = "UserName", all.x = TRUE)
colnames(p2p_bondera)[4:14] <- paste0("from_", colnames(p2p_bondera)[4:14])

p2p_bondera <- merge(p2p_bondera, bondora_sample[att_diffs],
               by.x = "to", by.y = "UserName", all.x = TRUE)
colnames(p2p_bondera)[15:25] <- paste0("to_", colnames(p2p_bondera)[15:25])

# --------------------------------------------------------------------------- #

bondera_sample_atts$name <- bondera_sample_atts$UserName

p2p_bondera_network <- igraph::graph_from_data_frame(
  d = p2p_bondera[c('from','to','weight')], directed = FALSE)

walktrap_comm <- snafun::extract_comm_walktrap(p2p_bondera_network)
snafun::g_summary(p2p_bondera_network)

par(mfrow = c(1, 2))

plot(p2p_bondera_network,
     main = "No Community Detection",
     edge.arrow.size = 0.3,
     edge.color = rgb(0,0,0, alpha = 0.15),
     vertex.frame.color = "black",
     vertex.label = NA,
     vertex.frame.size = 3,
     vertex.size = 5,
     vertex.shape = "circle",
     vertex.color = "cornsilk",
     edge.curved = FALSE,
     layout = igraph::layout.fruchterman.reingold)

plot(walktrap_comm, p2p_bondera_network,
     main = "Walktrap Communities",
     edge.arrow.size = 0.3,
     edge.color = rgb(0,0,0, alpha = 0.15),
     vertex.frame.color = "black",
     vertex.label = NA,
     vertex.frame.size = 3,
     vertex.size = 5,
     vertex.shape = "circle",
     vertex.color = "cornsilk",
     edge.curved = FALSE,
     layout = igraph::layout.fruchterman.reingold)

# Add Vertex Attributes
igraph::V(p2p_bondera_network)$Age <- bondera_sample_atts$Age[
  match(igraph::V(p2p_bondera_network)$name, bondera_sample_atts$name)]

bondora_sample$Education <- factor(bondora_sample$Education,
                                   levels = c(1, 2, 3, 4, 5),
                                   labels = c("Primary","Basic","Vocational",
                                              "Secondary", "Higher"))

bondora_sample$Gender <- factor(bondora_sample$Gender,
                                levels = c(0,1,2),
                                labels = c("Male","Female","Other"))

bondora_sample$MaritalStatus <- factor(bondora_sample$MaritalStatus,
                                       levels = c(1,2,3,4,5),
                                       labels = c("Married","Cohabitant",
                                                  "Single","Divorced","Widow"))

# Export the Network Object and Other Relevant Objects
saveRDS(p2p_bondera_network, 'resources/objects/p2p_network.RDS')
saveRDS(bondora_sample, 'resources/objects/bondora_sample.RDS')

# Descriptives
snafun::g_summary(p2p_network)
bootcamp::descriptives(bondora_df)

# --------------------------------------------------------------------------- #

barplot(table(bondora_sample$Gender),
        col = c("cornsilk2", "cornsilk4", "black"),
        main = "Gender Distribution",
        ylab = "Count")

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1)) 

barplot(table(bondora_sample$Gender),
        col = c("cornsilk2", "cornsilk4"),
        main = "Gender Distribution",
        ylab = "Count")

boxplot(bondora_sample$Age, main = "Age", 
        col = "cornsilk2", horizontal = TRUE)
boxplot(bondora_sample$LoanDuration, main = "Loan Duration", 
        col = "cornsilk2", horizontal = TRUE)
boxplot(bondora_sample$Amount, main = "Loan Amount", 
        col = "cornsilk2", horizontal = TRUE)
boxplot(bondora_sample$Interest, main = "Interest Rate", 
        col = "cornsilk2", horizontal = TRUE)

