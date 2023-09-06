# empty work space
rm(list = ls())

# get directories
dir_root <- getwd()
data_collection <- "experiment_ARK"
dir <- file.path(dir_root, data_collection)

##### --- read in in-task data ---- #####

file_task <- list.files(path = dir, pattern = "in-task")
task <- read.csv(file = file.path(dir, file_task))

# remove first two rows
task <- task[grepl("2023", task$StartDate),]
# remove irrelavant columns
task <- task[, c("pseudonym", "cog_load", 
                 "satisfaction_1", "satisfaction_2", "satisfaction_3", "satisfaction_4", 
                 "task_value_1", "task_value_2", "task_value_3")]
# standardise pseudonym
task$pseudonym <- toupper(task$pseudonym) # standardise
task$pseudonym <- gsub(" ", "", task$pseudonym)

# fix mismatch
task$pseudonym[task$pseudonym=="02N7"] <- "O2N7"
task$pseudonym[task$pseudonym=="02N8"] <- "O2N8"
task$pseudonym[task$pseudonym=="46P5"] <- "R6P5"
task$pseudonym[task$pseudonym=="S3S1"] <- "E3S1"
task$pseudonym[task$pseudonym=="BE3E"] <- "B3E3"
task$pseudonym[task$pseudonym=="E658"] <- "E6S8"
task$pseudonym[task$pseudonym=="O7H1"] <- "T0N3"

task <- subset(task, pseudonym != "1")
task <- subset(task, cog_load != "")

# transform cog load measure #

task[, 2:9] <- apply(task[, 2:9], 2, as.numeric)

# compute sum score for satisfaction with feedback
task$feedback_satisfaction <- rowSums(task[, grepl("satisfaction", names(task))])

# compute sum score for task value
task$task_value <- rowSums(task[, grepl("task_value", names(task))])

xlsx::write.xlsx(task, file.path(dir, "task_measures_ARK.xlsx"), sheetName="PROCESSED", col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)


##### --- merge with pretest data ---- #####

pretest <- xlsx::read.xlsx(file = file.path(dir, "pretest_ARK.xlsx"), sheetName = "SCALES")
df <- merge(pretest, task, by = "pseudonym", all = T)

# basic data checks
df$complete_sim <- ifelse(!is.na(df$cog_load), TRUE, FALSE)
df$nback_problematic <- ifelse(is.na(df$rate_accuracy) | df$rate_accuracy < .3, TRUE, FALSE)

# any missing task data?

nrow(pretest) - nrow(task)
