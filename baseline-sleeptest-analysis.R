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
dir_path <- vector()

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
          # Store the directory path:
          dir_path[full_path] <- base_dir_3
        }
      }
    }
  }

print ("Reading sleeptest result CSV files into a list")
# Read the sleeptest csv files into a list:
files_list <- lapply(file_names, read.csv)

# Data frame for actual rates:
# Pull out the values we need and merge into a single data frame
#  with a column of actual rates, a column for test type,
#  indexed against target rate:
# Set a blank data frame to put our results into:
df_sleeptimes = data.frame()
for (i in 1:length(files_list)) {
  d = files_list[[i]]
  #*** fill vector z1 with the test type:
  z1 <- rep("Sleep Test", length(files_list[[i]]$Target_Sleep))
  d$test_type <- z1
  # Set appropriate column names.
  colnames(d) <- c("Target_Sleep", "Actual_Sleep", "Discrepancy", "Percentage_Error", "Test_Type")
  # Accumulate the additional data rows:
  df_sleeptimes = rbind(df_sleeptimes, d)
}

# ============================= CHARTING ===============================

print("Creating chart 1...")
# Scatter lattice with panel per test type and R squared stat analysis:
scatter.lattice.discrepancy <- xyplot(Discrepancy ~ Target_Sleep | Test_Type, 
                          data = df_sleeptimes,
                          main="Absolute Sleep Error by Target Sleep Time",
                          xlab="Target Sleep (seconds)",
                          ylab="Actual Sleep difference from Target Sleep (seconds)",
                          as.table = TRUE)
p = scatter.lattice.discrepancy
print (p)

print("Creating chart 2...")
# Scatter lattice with panel per test type and R squared stat analysis:
scatter.lattice.percentage <- xyplot(Percentage_Error ~ Target_Sleep | Test_Type, 
                          data = df_sleeptimes,
                          main="Percentage Sleep Error by Target Sleep Time",
                          xlab="Target Sleep (seconds)",
                          ylab="Percentage Sleep difference from Target Sleep (seconds)",
                          as.table = TRUE)
p = scatter.lattice.percentage
print (p)
