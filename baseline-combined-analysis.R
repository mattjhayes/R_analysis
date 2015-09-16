# Analyse results from a filt unit test

# Run in rstudio with:

# source("~/analysis/baseline-combined-analysis.R")

# Imports:
libs <- c('ggplot2', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales')
lapply(libs, require, character.only = T)

# Base directory where results data is stored:
base_dir <- "~/results/baseline-combined/"

# Ask for the sub-directory that the results are in:
#test_dir_1 <- readline("What is name of directory?")
test_dir_1 <- "20150916202810"

base_dir_2 <- paste(base_dir, test_dir_1, sep = '')

print(paste0("Looking for test result data in ", base_dir_2))

files_dir_2 <- list.files(path=base_dir_2)

print(paste0("Found ", files_dir_2))

# Sanity check:
if (length(files_dir_2) < 1) {
  stop("No subdirectories?")
}

# ======================= filt load analysis:
# Read in the lg1.example.com-filt-*.csv files from the various
#  sub-directories:
file_names_filt <- vector()
test_types_filt <- vector()
dir_path_filt <- vector()

for (test_type in files_dir_2) {
  base_dir_3 <- paste(base_dir_2, test_type, sep = '/')
  files_dir_3 <- list.files(path=base_dir_3)
  for (test_timestamp in files_dir_3) {
    base_dir_4 <- paste(base_dir_3, test_timestamp, sep = '/')
    files_dir_4 <- list.files(path=base_dir_4)
    for (file_name in files_dir_4) {
      file_match <- grep("lg1\\.example\\.com\\-filt\\-.*\\-1.csv", file_name)
      if (length(file_match) > 0) {
        if (file_match == 1) {
          full_path_filt = paste(base_dir_4, file_name, sep = '/')
          # Append the full path of the file to the list
          file_names_filt  <- c(file_names_filt, full_path_filt)
          # Use test_types to hold mapping between full file path and type of test:
          test_types_filt[full_path_filt] <- test_type
          # Store the directory path:
          dir_path_filt[full_path_filt] <- base_dir_4
        }
      }
    }
  }
}

print ("Reading filt result CSV files into a list")
# Read the filt csv files into a list:
files_list_filt <- lapply(file_names_filt, read.csv)

# Data frame for filt times:
# Pull out the values we need and merge into a single data frame
#  with a column of retrieval times, a column for test type,
#  indexed against target rate:
# Set a blank data frame to put our results into:
df_filt = data.frame()
for (i in 1:length(files_list_filt)) {
  test_type <- unname(test_types_filt[i])
  dir_path <- unname(dir_path_filt[i])
  x <- files_list_filt[[i]]$time
  y <- files_list_filt[[i]]$previous.actual.rate.pps.
  #*** fill vector z1 with the test type:
  z1 <- rep(test_type, length(x))
  #*** fill vector z2 with the directory path:
  z2 <- rep(dir_path, length(x))
  d = data.frame(x, y, z1, z2)
  # Set appropriate column names.
  colnames(d) <- c("Time", "Previous_Actual_Rate", "Test_Type", "Dir_Path")
  # Accumulate the additional data rows:
  df_filt = rbind(df_filt, d)
}

# ===================== cxn-close analysis:
files_cxn_close <- vector()
test_types_cxn_close <- vector()
dir_path_cxn_close <- vector()

for (test_type in files_dir_2) {
  base_dir_3 <- paste(base_dir_2, test_type, sep = '/')
  files_dir_3 <- list.files(path=base_dir_3)
  for (test_timestamp in files_dir_3) {
    base_dir_4 <- paste(base_dir_3, test_timestamp, sep = '/')
    files_dir_4 <- list.files(path=base_dir_4)
    if (is.element("pc1.example.com-hort-cxn-close.csv", files_dir_4)) {
      full_path = paste(base_dir_4, "pc1.example.com-hort-cxn-close.csv", sep = '/')
      # Append the full path of the file to the list
      files_cxn_close <- c(files_cxn_close, full_path)
      # Use test_types to hold mapping between full file path and type of test:
      test_types_cxn_close[full_path] <- test_type
      # Store the directory path:
      dir_path_cxn_close[full_path] <- base_dir_4
    }
  }
}

print ("Reading hort client cxn-close result CSV files into a list")
# Read the pc1 connection close csv files into a list:
files_list_cxn_close <- lapply(files_cxn_close, read.csv)

# Data frame for cxn-close object retrieval times:
# Pull out the values we need and merge into a single data frame
#  with a column of retrieval times, a column for test type,
#  indexed against target rate:
# Set a blank data frame to put our results into:
df_cxn_close = data.frame()
for (i in 1:length(files_list_cxn_close)) {
  test_type <- unname(test_types_cxn_close[i])
  dir_path <- unname(dir_path_cxn_close[i])
  x <- files_list_cxn_close[[i]]$time
  y <- files_list_cxn_close[[i]]$pc1.cxn.close.retrieval.time
  #*** fill vector z1 with the test type:
  z1 <- rep(test_type, length(x))
  #*** fill vector z2 with the directory path:
  z2 <- rep(dir_path, length(x))
  d = data.frame(x, y, z1, z2)
  # Set appropriate column names.
  colnames(d) <- c("Time", "Object_Retrieval_Time", "Test_Type", "Dir_Path")
  # Accumulate the additional data rows:
  df_cxn_close = rbind(df_cxn_close, d)
}

# Add the filt load actual rate to the cxn-close data frame:
# First, need to load the correct filt data into a data frame:
for (dir_path in levels(df_cxn_close$Dir_Path)) {
  print(paste0("Dir path is ", dir_path))
}
#incol<- filt_results[,2] # select the column to search
#outcol <- 1 # select the element of the found row you want to get
#print("Test 1 results are")
#filt_results[ rev(order(incol<hort_timestamp))[1] ,outcol]




