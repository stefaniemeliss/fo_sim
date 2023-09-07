# function to compute accuracy
process_psytoolkit <- function(dir_in,# input folder directory
                               dir_out, # output folder directory
                               zipped = "data.zip" # name of zipped folder downloaded from psytoolkit
){
  
  #### upzip folder ####
  
  # choose data.zip folder 
  data_zip <- file.path(dir_in, zipped)
  
  # create temporary directory in which to unzip all files into
  tmp_dir <- file.path(dir_in, "tmp")
  
  if (dir.exists(tmp_dir)) {
    # delete tmp folder
    unlink(tmp_dir, recursive = T)
    dir.create(tmp_dir)
  } else {
    dir.create(tmp_dir)
  }
  
  # unzip folder
  unzip(data_zip, exdir = tmp_dir)
  
  #### process PSYTOOLKIT data (individual differences) ####
  
  # settings
  options(xlsx.datetime.format="YYYY/mm/dd hh:mm")
  
  # read in file
  df <- read.csv(file = file.path(tmp_dir, "data.csv"))
  
  # --- pseudonym ---
  
  df$pseudonym <- toupper(df$pseudonym_1) # standardise
  df$pseudonym <- gsub("[[:blank:]]", "", df$pseudonym) # standardise
  
  df <- subset(df, pseudonym != "") # remove all files that do not contain a pseudonym
  
  # --- write raw data (with fixed pseudonym) ---
  tmp <- df
  tmp$pseudonym_1 <- tmp$pseudonym
  tmp$pseudonym <- NULL
  
  # sort by time started
  tmp <- tmp[order(tmp$TIME_start), ]
  
  # save as sheet
  xlsx::write.xlsx(tmp, file.path(tmp_dir, "pretest_PHASE2.xlsx"), sheetName="RAW", col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)
  
  # --- process routing ---
  
  df$routing <- ifelse(df$select_1 == 1, "start of survey", 
                       ifelse(df$select_1 == 2, "jump to FO",
                              ifelse(df$select_1 == 3, "jump to BFI",
                                     ifelse(df$select_1 == 4, "jump to AQR",
                                            ifelse(df$select_1 == 5, "jump to DAMMQ", 
                                                   ifelse(df$select_1 == 6, "jump to nback", 
                                                          NA))))))
  
  # remove all participants that did not select any routing options (i.e., did not start the survey)
  df <- subset(df, !is.na(df$routing))
  
  # --- process timestamps as dt objects ---
  as.POSIXct(df$TIME_start, format = "%Y-%d-%m-%H-%M", tz = "Europe/Berlin")
  as.POSIXct(df$TIME_end, format = "%Y-%d-%m-%H-%M", tz = "Europe/Berlin")
  
  # convert timestamps (CEST) into POSIXct objects (in CEST)
  df$dt_start <- as.POSIXct(df$TIME_start, format = "%Y-%d-%m-%H-%M", tz = "Europe/Berlin")
  df$dt_end <- as.POSIXct(df$TIME_end, format = "%Y-%d-%m-%H-%M", tz = "Europe/Berlin")
  
  # convert POSIXct objects from CEST to BST
  df$dt_start <- format(df$dt_start,tz="Europe/London")
  df$dt_end <- format(df$dt_end,tz="Europe/London")
  
  # --- demographic data ---
  
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
  
  df$origin_ITT <- ifelse(df$origin_ITT_1 == 1, "UCL", 
                          ifelse(df$origin_ITT_1 == 2, "Northumbria",
                                 ifelse(df$origin_ITT_1 == 3, "other", NA)))
  
  
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
  df[, "nback_1"] <- ifelse(df[, "nback_1"] == "", NA, file.path(tmp_dir, "experiment_data", df[, "nback_1"]))
  # df[df[, "pseudonym"] == "R1R1", "nback_1"] <- NA # delete observation for ppt who reported to have had issues with the nback
  
  # process nback data
  tmp <- apply(df[grep("nback_1", names(df))], MARGIN = 1, compute_accuracy) # function defined in separate file
  tmp <- do.call(rbind, tmp) # convert list of dataframes to dataframe
  tmp <- subset(tmp, !is.na(nback_1)) # remove all rows with NAs
  
  # add to df
  df <- merge(df, tmp, by = "nback_1", all.x = T)
  nback_cols <- names(df)[grepl("nback|total_|rate_", names(df))] # overrates all counts, rates and file name
  
  # check all newly created columns
  start_col <- grep("TIME_total", names(df)) + 1
  data <- df[, start_col:ncol(df)]
  
  # sort by time started
  data <- data[order(data$dt_start), ]
  df <- df[order(df$dt_start), ]
  
  # write data
  xlsx::write.xlsx(df, file.path(tmp_dir, "pretest_PHASE2.xlsx"), sheetName="PROCESSED", col.names=TRUE, row.names=FALSE, append=TRUE, showNA=TRUE)
  xlsx::write.xlsx(data, file.path(tmp_dir, "pretest_PHASE2.xlsx"), sheetName="SCALES", col.names=TRUE, row.names=FALSE, append=TRUE, showNA=TRUE)
  
  # --- check for completeness ---
  
  # complete data checks
  complete <- data[, c("pseudonym", "routing", "dt_start", "dt_end", "origin_ITT", "fo", "conscientiousness", "m_app", "task_pleasure", "rate_accuracy")]
  names(complete) <-  c("pseudonym", "routing", "dt_start", "dt_end", "complete_demogs", "complete_fo", "complete_bfi", "complete_agq", "complete_dammq", "complete_nback")
  
  complete[, 5:10] <- apply(complete[, 5:10], MARGIN = 2, FUN = is_completed)
  
  # sort by time started
  complete <- complete[order(complete$dt_start), ]
  
  # write data
  xlsx::write.xlsx(complete, file.path(tmp_dir, "pretest_PHASE2.xlsx"), sheetName="IS_COMPLETED", col.names=TRUE, row.names=FALSE, append=TRUE, showNA=TRUE)
  
  # --- detect outliers ---
  
  
  
  # determine col names to check for outliers
  scales <- names(data)[grep("fo|conscient|m_a|p_a|task_|pref", names(data))]
  scales <- scales[!grepl("fo_", scales)]
  scales <- c(scales, "rate_accuracy")
  
  # for each scale, check for outliers
  outlier_iqr <- data[, c("pseudonym", "routing", "dt_start", "dt_end", scales)]
  outlier_iqr[, paste0(scales)] <- apply(data[scales], MARGIN = 2, is_outlier_iqr)
  
  # sort by time started
  outlier_iqr <- outlier_iqr[order(outlier_iqr$dt_start), ]
  
  # write data
  xlsx::write.xlsx(outlier_iqr, file.path(tmp_dir, "pretest_PHASE2.xlsx"), sheetName="IS_OUTLIER_IQR", col.names=TRUE, row.names=FALSE, append=TRUE, showNA=TRUE)
  
  # copy spreadsheet to output folder
  file.copy(file.path(tmp_dir, "pretest_PHASE2.xlsx"), file.path(dir_out, "pretest_PHASE2.xlsx"), overwrite = T)
}
