rm(list=ls())
# install.packages("devtools") # uncomment as appropriate

# --- load functions from github ---

# helpers
devtools::source_url("https://github.com/stefaniemeliss/fo_sim/blob/master/helper_functions.R?raw=TRUE")

# function to process nback data
devtools::source_url("https://github.com/stefaniemeliss/fo_sim/blob/master/compute_accuracy.R?raw=TRUE")

# function to process pretest data
devtools::source_url("https://github.com/stefaniemeliss/fo_sim/blob/master/process_psytoolkit.R?raw=TRUE")

# --- process psytoolkit data ---

# define function inputs

in_dir = "C:/Users/stefanie.meliss/Downloads/" # directory containing data.zip as downloaded from psytoolkit
out_dir = "C:/Users/stefanie.meliss/OneDrive - Ambition Institute/research_projects/Ark Feedback Orientation and Sims/Data" # directory to save processed data in
folder = "data.zip" # name of downloaded folder - this is default

# call function
process_psytoolkit(dir_in = in_dir, 
                   dir_out = out_dir,
                   zipped = folder)
