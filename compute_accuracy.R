# function to compute accuracy
compute_accuracy <- function(file_nback){
  
  # # debug:
  # file_nback <- file.path(dir, "psytoolkit", "experiment_data", "NBack_task_nonfinal.2023-08-09-1030.data.b2dbc2e6-81c3-4205-9aa1-2f153f423dc9.txt")
  
  if (is.na(file_nback)) {
    return(NA)
  } else {
    
    # read in file
    # creates df with nrow = total trials and ncol = 12
    data <- read.table(file = file_nback)
    # --- meaning of columns ---
    # V1 = Blocknumber (the number of the block, there are 3 blocks)
    # V2 = Trial number (the number of the trial)
    # V3 = Type of trial (1=a matching stimulus ; 0=a non-matching stimulus)
    # V4 = Score (1 means correct, 0 means incorrect)
    # V5 = Match (1 means participants matched correctly, 0 otherwise; only meaningful on match trials)
    # V6 = Miss (1 means participants missed, 0 otherwise; only meaningful on non-matching trials)
    # V7 = False Alarm (1 means participants wrongly pressed button, 0 otherwise; only meaningful on non-matching trials)
    # V8 = Reaction Time
    # V9 = Memory (a variable used internally)
    # V10 = Current letter (the current letter, a number between 1 and 15, representing letterA, etc)
    # V11 = nback1 (the letter 1 trial ago, a number between 1 and 15, representing with letterA, etc)
    # V12 = nback2 (the letter 2 trials ago, a number between 1 and 15, representing with letterA, etc)
    
    # --- from PsyToolKit code zip file: post task feedback ---
    # set &Total1 count ; select c3 == 1 && c1 == BLOCKNUMBER ## number of trials where there was a 2back item
    # set &Total2 count ; select c3 == 0 && c1 == BLOCKNUMBER ## number of trials where there was NOT a 3back item
    # set &Matches count     ; select c5 == 1 && c1 == BLOCKNUMBER ## number of matches
    # set &Misses count      ; select c6 == 1 && c1 == BLOCKNUMBER ## number of misses
    # set &FalseAlarms count ; select c7 == 1 && c1 == BLOCKNUMBER ## number of false alarms
    # set &MatchesPerc      expression &Matches     / &Total1 * 100.0
    # set &MissesPerc       expression &Misses      / &Total1 * 100.0
    # set &FalseAlarmsPerc  expression &FalseAlarms / &Total2 * 100.0
    
    
    # 1. discard practice trials (first block)
    data <- subset(data, V1 != 1) # V1 = block number
    
    # 2. create df
    out <- data.frame(nback_1 = file_nback) # for merge
    
    # 3. determine total counts
    out$total_nback <- sum(data$V3 == 1) # V3 = Type of trial (1=a matching stimulus)
    out$total_non <- sum(data$V3 == 0) # V3 = Type of trial (0=a non-matching stimulus)
    out$total_hits <- sum(data$V5 == 1) # V5 = Match (1 means participants matched correctly, 0 otherwise; only meaningful on match trials)
    out$total_misses <- sum(data$V6 == 1) # V6 = Miss (1 means participants missed, 0 otherwise; only meaningful on non-matching trials)
    out$total_fa <- sum(data$V7 == 1) # V7 = False Alarm (1 means participants wrongly pressed button, 0 otherwise; only meaningful on non-matching trials)
    
    out$total_responses <- out$total_hits + out$total_fa # how many times did ppt press key 'M'
    
    # 4. compute rates
    out$rate_nback <- out$total_nback / nrow(data)
    out$rate_non <- out$total_non / nrow(data)
    out$rate_hits <- out$total_hits / out$total_nback
    out$rate_misses <- out$total_misses / out$total_nback
    out$rate_fa <- out$total_fa / out$total_non
    out$rate_response <- out$total_responses / nrow(data)
    
    # 5. compute accuracy and return
    out$rate_accuracy <- out$rate_hits - out$rate_fa
    
    # 6. compute average reaction time for correct trials
    out$avg_rt_hits <- mean(data$V8[data$V5 == 1])
    
    
    # return data
    return(out)
  }
  
}


# test
# file_nback <- file.path(in_dir, "tmp", "experiment_data", "NBack_task_nonfinal.2023-09-14-1323.data.5614258c-212a-4cdd-8ee5-65be32c7f74d.txt")
# compute_accuracy(file_nback = file_nback)
