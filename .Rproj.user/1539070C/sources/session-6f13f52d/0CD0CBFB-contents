
# --------------------------------------------------------------------------- #

setwd("C:/Users/user/OneDrive/University & Studies/University/JADS DSBE/Social Network Analysis for DS/Group Project/SNA4DS_Group7_Project")

# Install relevant packages
install.packages("proxy")

# Import the .unl files
setwd('CZ_Parliament_8_Data/hl-2017ps')
mp_votes_1 <- read.delim('hl2017h1.unl', header = FALSE, sep = "|", fileEncoding = "Windows-1250")
mp_votes_2 <- read.delim('hl2017h2.unl', header = FALSE, sep = "|", fileEncoding = "Windows-1250")
mp_votes_3 <- read.delim('hl2017h3.unl', header = FALSE, sep = "|", fileEncoding = "Windows-1250")

house_votes <- read.delim('hl2017s.unl', header = FALSE, sep = "|", fileEncoding = "Windows-1250")
mp_vote_contest <- read.delim('hl2017x.unl', header = FALSE, sep = "|", fileEncoding = "Windows-1250")

setwd("..")
setwd('poslanci')
orgs <- read.delim('organy.unl', header = FALSE, sep = "|", fileEncoding = "Windows-1250")
people <- read.delim('osoby.unl', header = FALSE, sep = "|", fileEncoding = "Windows-1250")
people_mps <- read.delim('poslanec.unl', header = FALSE, sep = "|", fileEncoding = "Windows-1250")

setwd("..")
setwd('schuze')
meetings <- read.delim('bod_schuze.unl', header = FALSE, sep = "|", fileEncoding = "Windows-1250")

# --------------------------------------------------------------------------- #

setwd("~")

# Concatenate the votes together
mp_votes <- rbind(mp_votes_1, mp_votes_2, mp_votes_3)
rm(mp_votes_1, mp_votes_2, mp_votes_3)

# Column names for MP Vote Data
mp_votes_names <- c("id_mp","id_vote","result")
mp_votes[4] <- NULL
names(mp_votes) <- mp_votes_names

# Handle House Votes Data
house_votes[17:18] <- NULL
house_votes_names <- c("id_vote", "id_org", "id_meet", "id_session", "point",
                       "date", "time", "affirm", "negative", "abstained", "no_vote",
                       "mps_present", "quorum", "vote_type", "vote_result", "title")
names(house_votes) <- house_votes_names

# Extract all existing Meeting Numbers
meeting_numbers <- unique(house_votes$id_meet)

# Handle MP Contest Data
mp_vote_contest[4] <- NULL
mp_vote_contest_names <- c("id_vote", "id_person", "contest_type")
names(mp_vote_contest) <- mp_vote_contest_names

# Handle Organisations Data
orgs[11] <- NULL
org_names <- c("id_org", "id_parent_org", "org_type", "abb", "cz_name",
               "en_name", "established", "terminated", "priority", "cl_org_base")
names(orgs) <- org_names

# Remove any Organisations that were Terminated in the Past
orgs$terminated <- as.Date(orgs$terminated, format="%d.%m.%Y")
orgs$established <- as.Date(orgs$established, format="%d.%m.%Y")
orgs_clean <- subset(orgs, 
                     is.na(terminated) | format(terminated, "%Y") >= 2017)

# Handle People Data
people[10] <- NULL
people_names <- c("id_person", "person_title", "name", "surname", "title_after",
                  "dob", "gender", "change", "death")
names(people) <- people_names

# Handle People MP Data
people_mps[15:16] <- NULL
people_mps_names <- c("id_mp", "id_person","id_region", "id_org",
                       "id_period", "web", "street", "municipality",
                       "zip", "email", "phone", "fax", "phone_ps","facebook")
names(people_mps) <- people_mps_names

# Handle Meetings Data
#meetings[16] <- NULL
#meetings_name <- c("id_point", "id_meet", "id_print", "id_type", "point",
#                   "full_name", "complete_con", "note", "id_point_status",
#                   "invitation", "rj", "nop", "type_point", "id_sd", "abb")
#names(meetings) <- meetings_name
#meetings_subset <- subset(meetings, meetings$id_meet %in% meeting_numbers)
#meetings_subset <- meetings_subset[!duplicated(meetings_subset), ]

# --------------------------------------------------------------------------- #

# Combine People and MP Data with MP Votes Dataset
mp_subset <- c("id_mp", "id_person", "id_org")

# Assuming that every MP corresponds to a Person
mp_votes_combined <- merge(mp_votes, people_mps[mp_subset], 
                           by="id_mp", all.x = TRUE)

# Combine MP Votes with Party/Organisation
org_subset <- c("id_org", "abb", "en_name")
mp_votes_combined <- merge(mp_votes_combined, orgs_clean[org_subset], 
                           by="id_org", all.x = TRUE)

# Combine MP Votes with Voting Objection Data
mp_votes_combined <- merge(mp_votes_combined, mp_vote_contest, 
                           by=c("id_person", "id_vote"), all.x = TRUE)

# Combine MP Votes with General House Voting Data
house_votes_subset <- c("id_vote", "id_org","id_meet", "point",
                        "date", "affirm", "negative", "abstained", "no_vote",
                        "mps_present", "quorum", "vote_type", "vote_result")

mp_votes_combined <- merge(mp_votes_combined, house_votes[house_votes_subset],
                  by=c("id_vote"), all.x = TRUE)

# Combine MP Votes with Meeting Data
#mp_votes_combined <- merge(mp_votes_combined, meeting_subset[c("id_meet","point","id_type")],
#                           by=c("id_meet","point"),
#                           all.x=TRUE)

# Combine MP Votes with Personal Data
people_subset <- c("id_person", "name", "surname", "gender")
mp_votes_combined <- merge(mp_votes_combined, people[people_subset],
                           by = c("id_person"), all.x = TRUE)

# Clarify Column Names Post-Merge
names(mp_votes_combined)[names(mp_votes_combined) == "id_org.x"] <- "id_org"
names(mp_votes_combined)[names(mp_votes_combined) == "id_org.y"] <- "id_org_sponsor"
mp_votes_combined$result <- as.factor(mp_votes_combined$result)
mp_votes_combined$vote_result <- as.factor(mp_votes_combined$vote_result)

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

# Create subset with only Objections
mp_votes_combined$contest <- ifelse(!is.na(mp_votes_combined$contest_type), 1, 0)
mp_votes_combined_obj <- subset(mp_votes_combined,
                                !is.na(mp_votes_combined$contest_type))


# Save the main object for later reuse
saveRDS(mp_votes_combined, file = 'Resources/objects/mp_votes_combined.rds')
rm(mp_votes)

# --------------------------------------------------------------------------- #

# Reshape into m x n Matrix (Objections Matrix)
obj_votes_wide = reshape(data = mp_votes_combined_obj[c("id_mp", "id_vote", "contest")],
                 idvar= "id_mp",
                 v.names= c("contest"),
                 timevar= "id_vote",
                 direction = "wide")

# Reshape into m x n Matrix (Voting Presence Network (Roll Call))
votes_wide = reshape(data = mp_votes_combined[c("id_mp", "id_vote", "clean_votes")],
                         idvar= "id_mp",
                         v.names= c("clean_votes"),
                         timevar= "id_vote",
                         direction = "wide")

colnames(obj_votes_wide) <- gsub("^contest\\.", "", colnames(obj_votes_wide))
colnames(votes_wide) <- gsub("^clean_votes\\.", "", colnames(votes_wide))

votes_wide_zeros <- votes_wide
votes_wide_zeros[is.na(votes_wide)] <- 0

# --------------------------------------------------------------------------- #

vote_matrix <- as.matrix(votes_wide_zeros[,-1])
vote_matrix_cos <- proxy::simil(vote_matrix, method = "cosine", by_rows = TRUE)

# Change Negative Similarity to Positive for Absolute Agreement
# igraph cannot handle negative weights
vote_matrix_cos <- abs(vote_matrix_cos)

vote_sim_df <- as.data.frame(as.table(as.matrix(vote_matrix_cos)))

# Remove NAs since isolates are not allowed
vote_sim_df <- na.omit(vote_sim_df)

colnames(vote_sim_df) <- c("mp1", "mp2", "weight")

# Network is completely dense currently
# Choose threshold for similarity to avoid meaningless analysis
threshold <- 0.5
vote_sim_df_filtered <- subset(vote_sim_df, mp1 != mp2 & weight > threshold)

vote_network <- igraph::graph_from_data_frame(vote_sim_df_filtered, directed = FALSE)

plot(vote_network,
     edge.arrow.size = 0.3,
     edge.color = rgb(0,0,0, alpha = 0.15),
     vertex.frame.color = "black",
     vertex.label = NA,
     vertex.frame.size = 4,
     vertex.size = 6,
     vertex.shape = "square",
     vertex.color = "black",
     edge.curved = FALSE,
     layout = igraph::layout.fruchterman.reingold)

snafun::g_summary(vote_network)

