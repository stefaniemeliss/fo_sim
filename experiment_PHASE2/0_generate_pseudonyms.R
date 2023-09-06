# make reproducible
set.seed(1008) # first day of testing

# empty work space
rm(list = ls())

# get directories
dir_root <- getwd()
data_collection <- "experiment"
dir <- file.path(dir_root, data_collection)

# generate pseudonyms

first <- "Z"
second <- 0:9
third <- LETTERS[1:26]
forth <- 0:9


for (i in 1:1000) {
  pseudo <- paste0(first, sample(second, 1), sample(third, 1), sample(forth, 1))
  
  print(pseudo)
  
  if (i == 1) {
    pseudonyms <- pseudo
  } else {
    pseudonyms <- c(pseudonyms, pseudo)
  }
  
}

# remove duplicates
pseudonyms <- pseudonyms[!duplicated(pseudonyms)]

pseudonyms <- as.data.frame(pseudonyms)

write.csv(pseudonyms, file.path(dir, "pseudonyms.csv"), row.names = F)
