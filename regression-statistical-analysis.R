# Analyse results from a Statistical TC regression test

# Run in rstudio with:

# source("~/R_analysis/regression-statistical-analysis.R")

# Imports:
libs <- c('ggplot2', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales', 'zoo')
lapply(libs, require, character.only = T)

# Base directory where results data is stored:
base_dir <- "~/results/regression/"

# Get the old or new subdirectory from the user
nmeta_version <- readline("nmeta-statistical or nmeta2-statistical?")
base_dir <- paste(base_dir, nmeta_version, sep = '')

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

# =========================== CHART FACET (FRAME) LABELLER =============
fx_chart_facet_labeller <- function(var, value){
    value <- as.character(value)
    if (var=="Test_Type") { 
        value[value=="constrained-bw-iperf"] <- "Iperf Constrained Bandwidth"
        value[value=="unconstrained-bw-iperf"]   <- "Iperf Unconstrained Bandwidth Control"
    }
    return(value)
}

# ===================== MAIN PROGRAM ===================================
# Note: Iperf CSV format is:
# Time, local_IP, local_port, server_IP, server_port, duration,
#   Interval, Transfer, Bandwidth 

# Call function (see further up) to build file data:
files_pc <- fx_build_file_data("pc1.example.com-5555-iperf_result.txt", files_dir_2)

print ("Reading Iperf result CSV files into a list")
# Read the result CSV files into a list:
files_list_pc <- lapply(files_pc$files, read.csv, header=FALSE)

# Produce a data frame of just what we need in right format:
df_iperf_pc <- data.frame()
for (i in 1:length(files_list_pc)) {
    test_type <- unname(files_pc$test_types[i])
    dir_path <- unname(files_pc$dir_path[i])
    df_tmp <- data.frame(i)
    colnames(df_tmp)[1] = "Test Number"
    # Add in the 'y' column(s):
    df_tmp[2] <- files_list_pc[[i]][,5]
    df_tmp[3] <- files_list_pc[[i]][,9]
    colnames(df_tmp)[2] <- "TCP_Port"
    colnames(df_tmp)[3] <- "Bandwidth"
    #*** Add filled Test_Type column:
    df_tmp$Test_Type <- test_type
    #*** Add filled Hostname column:
    df_tmp$Hostname <- "pc1.example.com"
    #*** Add filled Dir_Path column:
    df_tmp$Dir_Path <- dir_path
    # Accumulate the additional data rows:
    df_iperf_pc = rbind(df_iperf_pc, df_tmp)
}

# ============================= CHARTING ===============================
# Plot results on a chart:
g <- ggplot(df_iperf_pc, aes(x=Hostname, y=Bandwidth)) +
    geom_point(shape=1) +
    facet_grid(. ~ Test_Type, labeller=fx_chart_facet_labeller) +
    scale_y_log10() + ggtitle("Statistical Regression Tests") +
    xlab("Hostname") +
    ylab("Bandwidth (bps - Log10)")
print (g)
