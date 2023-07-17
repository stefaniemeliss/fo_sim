##### --- set up --- #####

rm(list = ls())
dir <- getwd()
source("compute_accuracy.R")

##### --- read in data ---- #####

# data from the project stems from multuiple sources

# 1. CONSENT: qualtrics is used to collect consent

file_consent <- list.files(pattern = "consent")
consent <- read.csv(file = file_consent)

# remove first two rows
consent <- consent[grepl("2023", consent$StartDate),]
# remove irrelavant columns
consent <- consent[, c("name", "pseudonym", "consent", "booking_confirm", "StartDate", "EndDate")]
consent$pseudonym <- toupper(consent$pseudonym) # standardise

# 2. BOOKING: microsoft bookings is used to book a slot

file_bookings <- list.files(pattern = "Bookings")
bookings <- read.delim(file = file_bookings)

# get pseudonym
bookings$pseudonym <- gsub("{  Please enter your personal pseudonym: ", "", bookings$Custom.Fields, fixed = T)
bookings$pseudonym <- gsub("}", "", bookings$pseudonym, fixed = T)
bookings$pseudonym <- toupper(bookings$pseudonym) # standardise


# reduce to relevant columns
bookings <- bookings[, c("pseudonym", "Customer.Name", "Customer.Email", "Customer.Phone", "Date.Time" )]
names(bookings) <- c("pseudonym", "ppt_name", "ppt_email", "ppt_phone", "dt_booking")

# make booking a dt object
bookings$dt_booking <- as.POSIXct(bookings$dt_booking, format = "%d/%m/%Y %H:%M")


# 3. PSYTOOLKIT: psytoolkit was used to measure individual differences

df <- read.csv(file = file.path(dir, "psytoolkit", "data.csv"))
df <- subset(df, pseudonym_1 != "") # remove all files that do not contain a pseudonym
df <- subset(df, pseudonym_1 != "o5s9") # test run stef

# --- demographic data ---
df$pseudonym <- toupper(df$pseudonym_1) # standardise
df$complete <- ifelse(is.na(df$TIME_total), FALSE, TRUE)
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
df$task_persistence <- rowSums(df[, grepl("dammq_task_persistence_", names(df))])
df$task_absorption <- rowSums(df[, grepl("dammq_task_absorption_", names(df))])
df$preference_challenge <- rowSums(df[, grepl("dammq_preference_challenge_", names(df))])
df$task_pleasure <- rowSums(df[, grepl("dammq_task_pleasure_", names(df))])

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
write.csv(data, "pretest_data.csv", row.names = F)


# create master spreadsheet
# all.x creates a row for each ppt that had consented to participate
master <- merge(consent, bookings, by = "pseudonym", all.x = T)

# remove unneccasy cols
master$name <- NULL
master$consent <- NULL
master$booking_confirm <- NULL
master$StartDate <- NULL
master$EndDate <- NULL

master$ppt_phone <- paste0("0", master$ppt_phone) # add zero to phone number


# next to-do:
# write file
#write.csv(consent, "master_spreadsheet.csv", row.names = F)


