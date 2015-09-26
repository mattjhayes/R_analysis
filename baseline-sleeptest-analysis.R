# Analyse results from a sleeptest

# Run in rstudio with:

# source("~/R_analysis/baseline-sleeptest-analysis.R")

# Imports:
libs <- c('ggplot2', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales')
lapply(libs, require, character.only = T)

# Base directory where results data is stored:
base_dir <- "/home/bob/results/sleeptests/"

# Ask for the sub-directory that the results are in:
test_dir_1 <- readline("What is name of directory?")

base_dir_2 <- paste(base_dir, test_dir_1, sep = '')

print(paste0("Looking for test result data in ", base_dir_2))

files_dir_2 <- list.files(path=base_dir_2)

print(paste0("Found ", files_dir_2))

# Sanity check:
if (length(files_dir_2) < 1) {
  stop("No subdirectories?")
}

file_names <- vector()
test_types <- vector()

for (test_timestamp in files_dir_2) {
    base_dir_3 <- paste(base_dir_2, test_timestamp, sep = '/')
    files_dir_3 <- list.files(path=base_dir_3)
    for (file_name in files_dir_3) {
      file_match <- grep("lg1\\.example\\.com\\-sleeptest.csv", file_name)
      if (length(file_match) > 0) {
        if (file_match == 1) {
          full_path = paste(base_dir_3, file_name, sep = '/')
          # Append the full path of the file to the list
          file_names  <- c(file_names, full_path)
          # Use test_types to hold mapping between full file path and type of test:
          test_types[full_path] <- test_type
          # Store the directory path:
          dir_path[full_path] <- base_dir_3
        }
      }
    }
  }
}

print ("Reading sleeptest result CSV files into a list")
# Read the sleeptest csv files into a list:
files_list <- lapply(file_names, read.csv)
