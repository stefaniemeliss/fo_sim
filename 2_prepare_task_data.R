rm(list = ls())
dir <- getwd()
# make reproducible
set.seed(189) # number of sim slots available

##### --- read in in-task data ---- #####

file_task <- list.files(pattern = "in-task")
task <- read.csv(file = file_task)

# remove first two rows
task <- task[grepl("2023", task$StartDate),]
# remove irrelavant columns
task <- task[, c("pseudonym", "cog_load", 
                 "satisfaction_1", "satisfaction_2", "satisfaction_3", "satisfaction_4", 
                 "task_value_1", "task_value_2", "task_value_3")]
task$pseudonym <- toupper(task$pseudonym) # standardise

# transform cog load measure #

task[, 2:9] <- apply(task[, 2:9], 2, as.numeric)


task$feedback_satisfaction <- rowSums(task[, grepl("satisfaction", names(task))])

task$task_value <- rowSums(task[, grepl("task_value", names(task))])


psych::describe(task$task_value)
