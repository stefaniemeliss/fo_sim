# --- read in data ---- #

dir <- getwd()

# data from the project stems from multuiple sources

# 1. CONSENT: qualtrics is used to collect consent

file_consent <- list.files(pattern = "consent")
consent <- read.csv(file = file_consent)

# remove first two rows
consent <- consent[grepl("2023", consent$StartDate),]
# remove irrelavant columns
consent <- consent[, c("name", "pseudonym", "consent", "booking_confirm", "StartDate", "EndDate")]

# write file
write.csv(consent, "master_spreadsheet.csv", row.names = F)


# 2. BOOKING: microsoft bookings is used to book a slot

file_bookings <- list.files(pattern = "Bookings")
bookings <- read.delim(file = file_bookings)

# get pseudonym
bookings$pseudonym <- gsub("{  Please enter your personal pseudonym: ", "", bookings$Custom.Fields, fixed = T)
bookings$pseudonym <- gsub("}", "", bookings$pseudonym, fixed = T)

# reduce to relevant columns
bookings <- bookings[, c("pseudonym", "Customer.Name", "Customer.Email", "Customer.Phone", "Date.Time" )]


# 3. PSYTOOLKIT: psytoolkit was used to measure individual differences
df <- read.csv(file = file.path(dir, "psytoolkit", "data.csv"))

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




df[, grep("TIME_start", names(df)):ncol(df)]
