
# --------------------------------------------------------------------------- #
# Reset WD
setwd("../..")

# Install relevant packages
#install.packages("proxy") # for cosine similarity

# Import MP Voting Data
setwd('CZ_Parliament_8_Data/hl-2017ps')
mp_votes_1 <- read.delim('hl2017h1.unl', header = FALSE, sep = "|", 
                         fileEncoding = "Windows-1250")
mp_votes_2 <- read.delim('hl2017h2.unl', header = FALSE, sep = "|", 
                         fileEncoding = "Windows-1250")
mp_votes_3 <- read.delim('hl2017h3.unl', header = FALSE, sep = "|", 
                         fileEncoding = "Windows-1250")

# Import Data Related to Overall Voting
house_votes <- read.delim('hl2017s.unl', header = FALSE, sep = "|", 
                          fileEncoding = "Windows-1250")
mp_vote_contest <- read.delim('hl2017x.unl', header = FALSE, sep = "|", 
                              fileEncoding = "Windows-1250")

# Import Data Related to MPs Individually
setwd("..")
setwd('poslanci')
orgs <- read.delim('organy.unl', header = FALSE, sep = "|", 
                   fileEncoding = "Windows-1250")
people <- read.delim('osoby.unl', header = FALSE, sep = "|", 
                     fileEncoding = "Windows-1250")
people_mps <- read.delim('poslanec.unl', header = FALSE, sep = "|", 
                         fileEncoding = "Windows-1250")

# Import Data Related to Backgrounds
setwd("..")
backgrounds <- readxl::read_excel('candidate_lists/2017.xlsx')

# --------------------------------------------------------------------------- #
# Reset WD
setwd("./")

# Concatenate the votes together
mp_votes <- rbind(mp_votes_1, mp_votes_2, mp_votes_3)
rm(mp_votes_1, mp_votes_2, mp_votes_3) # Save Memory

# Clean MP Vote Data
mp_votes[4] <- NULL
names(mp_votes) <- c("id_mp","id_vote","result")

# Handle House Votes Data
house_votes[17:18] <- NULL
names(house_votes) <- c("id_vote", "id_org", "id_meet", "id_session", 
                        "point", "date", "time", "affirm", "negative", 
                        "abstained", "no_vote", "mps_present", "quorum", 
                        "vote_type", "vote_result", "title")

# Handle MP Contest Data
mp_vote_contest[4] <- NULL
names(mp_vote_contest) <- c("id_vote", "id_person", "contest_type")

# Handle Organisations Data
orgs[11] <- NULL
names(orgs) <- c("id_org", "id_parent_org", "org_type", "abb", "cz_name",
                 "en_name", "established", "terminated", "priority", 
                 "cl_org_base")

# Remove any Organisations that were Terminated in the Past
orgs$terminated <- as.Date(orgs$terminated, format="%d.%m.%Y")
orgs$established <- as.Date(orgs$established, format="%d.%m.%Y")
orgs_clean <- subset(orgs, 
                     is.na(terminated) | format(terminated, "%Y") >= 2017)

# Handle People Data
people[10] <- NULL
names(people) <- c("id_person", "person_title", "name", "surname", 
                   "title_after","dob", "gender", "change", "death")

# Handle People MP Data
people_mps[15:16] <- NULL
names(people_mps) <- c("id_mp", "id_person","id_region", "id_org",
                       "id_period", "web", "street", "municipality",
                       "zip", "email", "phone", "fax", "phone_ps", "facebook")

# --------------------------------------------------------------------------- #

# Combine People and MP Data with MP Votes Dataset
mp_votes_combined <- merge(mp_votes, 
                           people_mps[c("id_mp", "id_person", "id_org")], 
                           by="id_mp", all.x = TRUE)

# Combine MP Votes with Party/Organisation
mp_votes_combined <- merge(mp_votes_combined, 
                           orgs_clean[c("id_org", "abb", "en_name")], 
                           by="id_org", all.x = TRUE)

# Combine MP Votes with Voting Objection Data
mp_votes_combined <- merge(mp_votes_combined, mp_vote_contest, 
                           by=c("id_person", "id_vote"), all.x = TRUE)

# Combine MP Votes with General House Voting Data
mp_votes_combined <- merge(mp_votes_combined, 
                           house_votes[c("id_vote", "id_org","id_meet", 
                                         "point", "date", "affirm", "negative", 
                                         "abstained", "no_vote","mps_present", 
                                         "quorum", "vote_type", "vote_result")],
                           by=c("id_vote"), all.x = TRUE)

# Combine MP Votes with Personal Data
mp_votes_combined <- merge(mp_votes_combined, 
                           people[c("id_person", "name", "surname", "gender")],
                           by = c("id_person"), all.x = TRUE)

# Clarify Column Names Post-Merge
names(mp_votes_combined)[names(mp_votes_combined) == "id_org.x"] <- "id_org"
names(mp_votes_combined)[names(mp_votes_combined) == "id_org.y"] <- "id_org_sponsor"

# Clarify Voting Results in Machine Understandable Way
vote_mapping_presence <- c("A" = 1, "B" = 1, "N" = 1, "C" = 0, 
                           "F" = 1, "@" = 0, "M" = 0, "W" = 1, "K" = 0)

vote_mapping_agreement <- c("A" = 1, "B" = -1, "N" = -1, "C" = 0, 
                            "F" = 1, "@" = 0, "M" = 0, "W" = 1, "K" = 0)

mp_votes_combined$clean_votes <- vote_mapping_agreement[mp_votes_combined$result]
mp_votes_combined$clean_votes_presence <- vote_mapping_presence[mp_votes_combined$result]

# Remove Votes where Less than 30 MPs Participated
mp_votes_combined <- subset(mp_votes_combined, 
                            mp_votes_combined$mps_present > 20)

# --------------------------------------------------------------------------- #

# Reshape into m x n Matrix (Voting Presence Network (Roll Call))
votes_wide = reshape(data = mp_votes_combined[c("id_mp", "id_vote", "clean_votes")],
                     idvar= "id_mp",
                     v.names= c("clean_votes"),
                     timevar= "id_vote",
                     direction = "wide")

colnames(votes_wide) <- gsub("^clean_votes\\.", "", colnames(votes_wide))

# We will count any NAs as non-participating MPs
votes_wide_zeros <- votes_wide
votes_wide_zeros[is.na(votes_wide)] <- 0

# --------------------------------------------------------------------------- #

# Create the cosine similarity Matrix
vote_matrix <- as.matrix(votes_wide_zeros[,-1])
vote_matrix_cos <- proxy::simil(vote_matrix, method = "cosine", by_rows = TRUE)

# Change Negative Similarity to Positive for Absolute Agreement
# igraph cannot handle negative weights
vote_matrix_cos <- abs(vote_matrix_cos)
vote_sim_df <- as.data.frame(as.table(as.matrix(vote_matrix_cos)))

# Count and Remove NAs since isolates are not allowed
print(paste("NA Count: ", sum(is.na(vote_sim_df))))
vote_sim_df <- na.omit(vote_sim_df)
colnames(vote_sim_df) <- c("from", "to", "weight")

# Network is completely dense currently
# Choose threshold for similarity to avoid meaningless analysis
threshold <- 0.5
vote_sim_df_filtered <- subset(vote_sim_df, from != to & weight > threshold)

vote_network <- igraph::graph_from_data_frame(vote_sim_df_filtered, 
                                              directed = FALSE)
snafun::g_summary(vote_network)

# --------------------------------------------------------------------------- #
