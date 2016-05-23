# Analyse results from a timeliness no-load statistical test

# Run in rstudio with:

# source("~/R_analysis/cp-timely-load-statistical.R")

# Imports:
libs <- c('ggplot2', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales', 'zoo', 'scales')
lapply(libs, require, character.only = T)

# Base directory where results data is stored:
base_dir <- "~/results/timeliness/controlplane"

# Ask for the sub-directory that the results are in:
test_dir_1 <- readline("What is name of directory?")

base_dir_2 <- paste(base_dir, test_dir_1, sep = '/')

print(paste0("Looking for test result data in ", base_dir_2))

files_dir_2 <- list.files(path=base_dir_2)

print(paste0("Found ", files_dir_2))

# Sanity check:
if (length(files_dir_2) < 1) {
  stop("No subdirectories?")
}

# =============================== BUILD FILE DATA FUNCTION =============
fx_build_file_data <- function(file_name, files_dir_2) {
    # Use this to build file data prior to importing CSVs
    # Pass it the name of the CSV file and base directory and it will trawl
    # the directory structure and return list of 3 vectors that are needed
    files_vector <- vector()
    test_types_vector <- vector()
    dir_path_vector <- vector()

    for (test_type in files_dir_2) {
        base_dir_3 <- paste(base_dir_2, test_type, sep = '/')
        files_dir_3 <- list.files(path=base_dir_3)
        for (test_timestamp in files_dir_3) {
            base_dir_4 <- paste(base_dir_3, test_timestamp, sep = '/')
            files_dir_4 <- list.files(path=base_dir_4)
            if (is.element(file_name, files_dir_4)) {
                full_path = paste(base_dir_4, file_name, sep = '/')
                # Append the full path of the file to the list
                files_vector <- c(files_vector, full_path)
                # Use test_types to hold mapping between full file path and type of test:
                test_types_vector[full_path] <- test_type
                # Store the directory path:
                dir_path_vector[full_path] <- base_dir_4
            }
        }
    }
    returnList <- list("files" = files_vector,
                       "test_types" = test_types_vector,
                       "dir_path" = dir_path_vector)
    return(returnList)
}

# ===================== MAIN PROGRAM ===================================

# Call function (see further up) to build file data:
files_cp <- fx_build_file_data("post_process_control_plane_time_delta.txt", files_dir_2)

print ("Reading result CSV files into a list")
# Read the result CSV files into a list:
files_list_cp <- lapply(files_cp$files, read.csv, header=FALSE)

df_cp <- data.frame()
for (i in 1:length(files_list_cp)) {
    # Read data frame from list
    df_temp <- files_list_cp[[i]]
    df_temp$TestType <- unname(files_cp$dir_path[i])
    names(df_temp) <- c("TestType", "LoadRate", "CPMAC", "DirPath")
    print ("Doing rbind to accumulate row")
    df_cp = rbind(df_cp, df_temp)
}
