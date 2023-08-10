##### --- set up --- #####

# empty work space
rm(list = ls())

# get directories
dir_root <- getwd()
data_collection <- "experiment"
dir <- file.path(dir_root, data_collection)

# read in files
source(file.path(dir_root, "compute_accuracy.R"))
source(file.path(dir_root, "passwords.R"))
options(xlsx.datetime.format="YYYY/mm/dd hh:mm")

##### --- read in data ---- #####

# data from the project stems from multiple sources

# 1. CONSENT: qualtrics is used to collect consent

file_consent <- list.files(path = dir, pattern = "consent")
consent <- read.csv(file = file.path(dir, file_consent))

# remove first two rows
consent <- consent[grepl("2023", consent$StartDate),]

# remove all participants not consentting
consent <- subset(consent, consent == "I consent to take part in the research project described above")

# remove irrelavant columns
consent <- consent[, c("name", "pseudonym", "consent", "booking_confirm", "StartDate", "EndDate")]
names(consent)[names(consent) == "name"] <- "ppt_name_signed"
consent$pseudonym <- toupper(consent$pseudonym) # standardise
consent$pseudonym <- gsub("[[:blank:]]", "", consent$pseudonym) # standardise

# overwrite inconsistent pseudonym
consent$pseudonym[consent$pseudonym == "O7R1"] <- "U7R1"

# 2. BOOKING: microsoft bookings is used to book a slot

file_bookings <- list.files(path = dir, pattern = "Bookings")
bookings <- read.delim(file = file.path(dir, file_bookings))

# get pseudonym
bookings$pseudonym <- gsub("[[:punct:]]", "", bookings$Custom.Fields) # remove all speical characters
bookings$pseudonym <- gsub("  Please enter your personal pseudonym ", "", bookings$pseudonym)
bookings$pseudonym <- toupper(bookings$pseudonym) # standardise
bookings$pseudonym <- gsub("[[:blank:]]", "", bookings$pseudonym) # standardise

# remove all rows that do not contain a pseudonym
bookings <- subset(bookings, nchar(pseudonym) > 0)

# reduce to relevant columns
bookings <- bookings[, c("pseudonym", "Customer.Name", "Customer.Email", "Customer.Phone", "Date.Time" )]
names(bookings) <- c("pseudonym", "ppt_name", "ppt_email", "ppt_phone", "start_lobby")

# make booking a dt object
bookings$start_lobby
bookings$start_lobby <- as.POSIXct(bookings$start_lobby, format = "%d/%m/%Y %H:%M")
bookings$start_lobby
bookings$start_lobby <- as.POSIXct(strptime(bookings$start_lobby, "%Y-%m-%d %H:%M:%S"), tz = "UTC")

# 3. PSYTOOLKIT: psytoolkit was used to measure individual differences

df <- read.csv(file = file.path(dir, "psytoolkit", "data.csv"))

# --- demographic data ---
df$pseudonym <- toupper(df$pseudonym_1) # standardise
df$pseudonym <- gsub("[[:blank:]]", "", df$pseudonym) # standardise

df <- subset(df, pseudonym != "") # remove all files that do not contain a pseudonym
df <- subset(df, pseudonym != "TEST") # remove all files that do not contain a pseudonym

df$complete_psytoolkit <- ifelse(is.na(df$TIME_total), FALSE, TRUE)
df$age <- df$age
df$gender <- ifelse(df$gender_1 == 1, "male", 
                    ifelse(df$gender_1 == 2, "female",
                           ifelse(df$gender_1 == 3, "different", NA)))
df$ethnicity <- ifelse(df$ethnicity_1 == 1, "Asian", 
                       ifelse(df$ethnicity_1 == 2, "Black",
                              ifelse(df$ethnicity_1 == 3, "Mixed",
                                     ifelse(df$ethnicity_1 == 4, "White",
                                            ifelse(df$ethnicity_1 == 5, "Other", NA)))))

df$education <- ifelse(df$education_1 == 1, "undergraduate", 
                       ifelse(df$education_1 == 2, "postgraduate",
                              ifelse(df$education_1 == 3, "doctorate", NA)))

df$ta_experience <- ifelse(df$ta_experience_1 == 1, TRUE, 
                           ifelse(df$ta_experience_1 == 2, FALSE, NA))


df$school_level <- ifelse(df$school_level_1 == 1, "primary", 
                       ifelse(df$school_level_1 == 2, "secondary",
                              ifelse(df$school_level_1 == 3, "unknown", NA)))

df$school_ark <- ifelse(df$school_ark_1 == 1, "Ark", 
                          ifelse(df$school_ark_1 == 2, "non-Ark",
                                 ifelse(df$school_ark_1 == 3, "unknown", NA)))

# fix mismatch
df$pseudonym[df$pseudonym == "17D1"] <- "I7D1"


# --- compute scales ---

# 1. FEEDBACK ORIENTATION SCALE
df$fo <- rowSums(df[, grepl("fos_", names(df))]) # sum score all items
df$fo_utility <- rowSums(df[, grepl("fos_", names(df))][1:5]) # sum score item 1:5
df$fo_accountability <- rowSums(df[, grepl("fos_", names(df))][6:10]) # sum score item 1:5
df$fo_awareness <- rowSums(df[, grepl("fos_", names(df))][11:15]) # sum score item 1:5
df$fo_efficacy <- rowSums(df[, grepl("fos_", names(df))][16:20]) # sum score item 1:5

# 2. CONSCIENTIOUSNESS (1R, 2R, 3, 4, 5R, 6R, 7, 8, 9, 10R, 11, 12R)

# recode list of items
items <- c("bfi_conscientiousness_1", "bfi_conscientiousness_2", "bfi_conscientiousness_5", 
           "bfi_conscientiousness_6", "bfi_conscientiousness_10", "bfi_conscientiousness_12")
df[items] <- apply(df[items], MARGIN = 2, function(x) 6 - x ) # recoding is done by subtracting the current value from 6, e.g., 6 - 1 = 5 or 6 - 5 = 1
df$conscientiousness <- rowSums(df[, grepl("bfi_conscientiousness_", names(df))]) # sum score all items after re-coding

# 3. ACHIEVEMENT GOAL QUESTIONNAIRE
df$m_app <- df$agq_1 + df$agq_7 + df$agq_3
df$m_av <- df$agq_5 + df$agq_11 + df$agq_9
df$p_app <- df$agq_4 + df$agq_2 + df$agq_8
df$p_av <- df$agq_12 + df$agq_10 + df$agq_6


# 4. DAMMQ

items <- names(df)[grepl("dammq", names(df))]

df$task_persistence <- rowSums(df[, items[1:8]])
df$task_absorption <- rowSums(df[, items[9:12]])
df$preference_challenge <- rowSums(df[, items[13:16]])
df$task_pleasure <- rowSums(df[, items[17:20]])

# 5. n-back task

# overwrite nback file with file path
df[, "nback_1"] <- ifelse(df[, "nback_1"] == "", NA, file.path(dir, "psytoolkit", "experiment_data", df[, "nback_1"]))

# compute accuracy
df$nback_accuracy <- apply(df[grep("nback_1", names(df))], MARGIN = 1, compute_accuracy) # function defined in separate file


##### --- extract data --- #####

# check all newly created columns
start_col <- grep("TIME_total", names(df)) + 1
data <- df[, start_col:ncol(df)]

# write data
write.csv(data, file.path(dir, "pretest_data.csv"), row.names = F)


# --- create master spreadsheet ---

# all.x creates a row for each ppt that had consented to participate
master <- merge(consent, bookings, by = "pseudonym", all = T)

# fix mismatch
master$ppt_name_signed[grep("768@g", master$ppt_email)] <- NA
master$pseudonym[grep("768@g", master$ppt_email)] <- paste(master$pseudonym[grep("768@g", master$ppt_email)], " ")

# remove unneccasy cols
master$booking_confirm <- NULL
master$StartDate <- NULL
master$EndDate <- NULL
master$ppt_phone <- ifelse(substr(master$ppt_phone, 1, 1) == 7, paste0("(+44)", master$ppt_phone), paste0(master$ppt_phone)) # add zero to phone number
master$ppt_phone <- ifelse(substr(master$ppt_phone, 1, 2) == 44, paste0("(", master$ppt_phone), paste0(master$ppt_phone)) # add zero to phone number
master$ppt_phone <- gsub("(44", "(+44)", master$ppt_phone, fixed = T)


# read in slot allocation
slots <- read.csv(file.path(dir, "slot_allocation.csv"))
slots <- subset(slots, slots$Start.Lobby != "")
slots$start_lobby <- paste(slots$Date, slots$Start.Lobby)

slots$start_lobby
slots$start_lobby <- as.POSIXct(slots$start_lobby, format = "%d/%m/%Y %H:%M:%S")
slots$start_lobby <- as.POSIXct(strptime(slots$start_lobby, "%Y-%m-%d %H:%M:%S"), tz = "UTC")

slots$start_lobby

slots$start_simulator <- paste(slots$Date, slots$Start.Simulator)
slots$start_simulator <- as.POSIXct(slots$start_simulator, format = "%d/%m/%Y %H:%M:%S")
slots$start_simulator <- as.POSIXct(strptime(slots$start_simulator, "%Y-%m-%d %H:%M:%S"), tz = "UTC")

slots <- slots[, c("start_lobby", "lobby", "start_simulator", "coach")]

# merge slots (DEBUG: tmp) and master
master <- merge(master, slots, by = "start_lobby", all.x = T)

# complete data checks
tmp <- data[, c("pseudonym", "complete_psytoolkit", "school_ark", "fo", "conscientiousness", "m_app", "task_pleasure", "nback_accuracy")]
names(tmp) <-  c("pseudonym", "complete_psytoolkit", "complete_demogs", "complete_fo", "complete_bfi", "complete_agq", "complete_dammq", "complete_nback")
complete <- function(x) {
  return(ifelse(is.na(x), F, T))
}
tmp[,3:8] <- apply(tmp[,3:8], MARGIN = 2, FUN = complete)

# merge with master
master <- merge(master, tmp, by = "pseudonym", all = T)

# add additional columns
master$show <- ""
master$consent_chased <- ifelse(is.na(master$ppt_name_signed) == T, FALSE, NA)
master$booking_chased <- ifelse(is.na(master$ppt_name) == T, FALSE, NA)
master$pretest_chased <- ifelse(master$complete_psytoolkit == F, FALSE, NA)
master$received_vids <- ""
master$received_transcripts <- ""
master$reviewed_vids <- ""
master$reviewed_transcript <- ""
master$coded_vids <- ""
master$comments_vids <- ""
master$comments_transcripts <- ""
master$comments_coding <- ""
master$comments_general <- ""

# load in old comments if exist
if (file.exists(file.path(dir, "master_spreadsheet.csv"))) {
  comments <- read.csv(file.path(dir, "master_spreadsheet.csv"))
  
  
  first_col <- grep("show", names(comments))
  
  comments[, c(1, first_col:ncol(comments))]
  
}

# re-order columns
master <- master[, c("pseudonym", "ppt_name_signed", "ppt_name", "ppt_email", "ppt_phone", "start_lobby", "lobby", "start_simulator", "coach", 
                     "complete_psytoolkit", "consent_chased", "booking_chased", "pretest_chased", 
                     "complete_demogs", "complete_fo", "complete_bfi", "complete_agq", "complete_dammq", "complete_nback", 
                     "show", "received_vids", "received_transcripts", "reviewed_vids", "reviewed_transcript", "coded_vids", 
                     "comments_vids", "comments_transcripts", "comments_coding", "comments_general")]

# order sim slots chronologically
master <- master[order(master$start_simulator), ]


# create dictionary
dict <- data.frame(variable = names(master))
dict$explanation <- c("Participant pseudonym, USE FOR FILE NAMES",
                      "Name (as provided in consent form)",
                      "Name (as provided in bookings form)",
                      "Email (as provided in bookings form)",
                      "Phone number (as provided in bookings form)",
                      "Scheduled start time of LOBBY session in format YYYY/MM/DD hh:mm",
                      "Assigned staff for lobby",
                      "Scheduled start time of simulator session in format YYYY/MM/DD hh:mm",
                      "Assigned staff for coaching",
                      "Data validation: is pretest complete",
                      "Data validation: has incomplete data been chased; NA if consent complete",
                      "Data validation: has incomplete data been chased; NA if booking complete",
                      "Data validation: has incomplete data been chased; NA if pretest complete",
                      rep("Data validation for routing as necessary: is pretest element complete", 6),
                      
                      "record of whether session took place",
                      
                      rep("Data validation: has simulator data been received", 2),
                      rep("Data validation/fidelity: has simulator data been reviewed", 2),
                      "Data validation: has video been coded",
                      rep("Data validation/fidelity: comments and notes", 4))
dict$levels_1 <- ""
dict$levels_2 <- ""
dict$levels_3 <- ""
dict$levels_4 <- "" 
dict$levels_5 <- "" 

dict$levels_1[dict$variable == "show"] <- "show"
dict$levels_2[dict$variable == "show"] <- "no show"
dict$levels_3[dict$variable == "show"] <- "cancelled"
dict$levels_4[dict$variable == "show"] <- "rebooked"

dict$levels_1[c(grep("received", dict$variable), grep("reviewed", dict$variable), grep("coded", dict$variable))] <- TRUE
dict$levels_2[c(grep("received", dict$variable), grep("reviewed", dict$variable), grep("coded", dict$variable))] <- FALSE
dict$levels_3[c(grep("received", dict$variable), grep("reviewed", dict$variable), grep("coded", dict$variable))] <- NA

xlsx::write.xlsx(master, file.path(dir, "master_spreadsheet.xlsx"), sheetName="Sheet1", col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE, password=password_master)
xlsx::write.xlsx(master, file.path(dir, "master_spreadsheet.xlsx"), sheetName="participant_info", col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)
xlsx::write.xlsx(dict, file.path(dir, "master_spreadsheet.xlsx"), sheetName="columns_explained", col.names=TRUE, row.names=FALSE, append=TRUE, showNA=TRUE)




##### --- randomisation --- #####

# create separate data frame to randomise
rand <- master[, c("start_simulator", "pseudonym", "ppt_name", "coach")]

allocation <- read.delim(file.path(dir, "randomised_group_assignment_n160.txt"), header = F)

rand$treatment_arm <- allocation[1:nrow(master), "V1"] # only select the first nrow(master) group assignments and add

# DEBUG/TEST
tapply(rand$pseudonym, rand$treatment_arm, length)

# add columns to capture feedback
rand$show <- ""
rand$feedback_1_core_a <- ""
rand$feedback_1_core_b <- ""
rand$feedback_1_core_c <- ""
rand$feedback_2_core_a <- ""
rand$feedback_2_core_b <- ""
rand$feedback_2_core_c <- ""
rand$comment <- ""

# order sim slots chronologically
rand <- rand[order(rand$start_simulator), ]

dict <- data.frame(variable = names(rand))
dict$explanation <- c("Scheduled start time of simulator session in format YYYY/MM/DD hh:mm",
                      "Participant pseudonym, USE FOR FILE NAMES",
                      "Name of participant, as info for coaches",
                      "assigned coach",
                      "which condition the participant is in",
                      "record of whether session took place",
                      rep("coach use only: was core practice shown/which script was used", 6),
                      "coach use only: any comments related to session, incl.  problems")
dict$levels_1 <- ""
dict$levels_2 <- ""
dict$levels_3 <- ""
dict$levels_4 <- ""
dict$levels_5 <- "" 

dict$levels_1[dict$variable == "show"] <- "show"
dict$levels_2[dict$variable == "show"] <- "no show"
dict$levels_3[dict$variable == "show"] <- "cancelled"
dict$levels_4[dict$variable == "show"] <- "rebooked"

dict$levels_1[grep("feedback", dict$variable)] <- "did"
dict$levels_2[grep("feedback", dict$variable)] <- "did not"
dict$levels_3[grep("feedback", dict$variable)] <- "partial"


# --- save all files --- #


# save file ***PASSWORD PROTECTED****
xlsx::write.xlsx(rand, file.path(dir, "simulator_slots.xlsx"), sheetName="Sheet1", col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE, password=password_rand)
xlsx::write.xlsx(rand, file.path(dir, "simulator_slots.xlsx"), sheetName="Slots", col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)
xlsx::write.xlsx(dict, file.path(dir, "simulator_slots.xlsx"), sheetName="columns_explained", col.names=TRUE, row.names=FALSE, append=TRUE, showNA=TRUE)



