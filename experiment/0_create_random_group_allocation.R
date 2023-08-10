# make reproducible
set.seed(1008) # first day of testing

# empty work space
rm(list = ls())

# get directories
dir_root <- getwd()
data_collection <- "experiment"
dir <- file.path(dir_root, data_collection)

# create vector with all group names
group <- c("decomposed-recomposed", "holistic-holistic")

for (i in 1:200) {
  # replicate the group object to create vector that matches sum of participants that we hope to recruit plus attruition
  group_vec <- rep_len(group, length.out = 10)
  
  # sample without replacement from group vec --> RANDOMISATION
  tmp <- sample(group_vec, size = 10, replace = F)
  
  if (i == 1) {
    group_rand <- tmp
  } else {
    group_rand <- c(group_rand, tmp)
  }
  
}


write(group_rand, file.path(dir, "randomised_group_assignment_n200.txt"))
