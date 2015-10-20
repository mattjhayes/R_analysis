# Analyse results from a static TC regression test

# Run in rstudio with:

# source("~/R_analysis/regression-static-analysis.R")

# Imports:
libs <- c('ggplot2', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales', 'zoo')
lapply(libs, require, character.only = T)

# Base directory where results data is stored:
base_dir <- "~/results/regression/nmeta-static/"

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
        value[value=="constrained-bw-tcp1234"] <- "TCP-1234 Constrained Bandwidth"
        value[value=="constrained-bw-tcp5555"]   <- "TCP-5555 Constrained Bandwidth"
    }
    return(value)
}

# ===================== MAIN PROGRAM ===================================
# Note: Iperf CSV format is:
# Time, local_IP, local_port, server_IP, server_port, duration,
#   Interval, Transfer, Bandwidth 

# Call function (see further up) to build file data:
files_1234 <- fx_build_file_data("pc1.example.com-1234-iperf_result.txt", files_dir_2)
files_5555 <- fx_build_file_data("pc1.example.com-5555-iperf_result.txt", files_dir_2)

print ("Reading Iperf result CSV files into a list")
# Read the result CSV files into a list:
files_list_1234 <- lapply(files_1234$files, read.csv, header=FALSE)
files_list_5555 <- lapply(files_5555$files, read.csv, header=FALSE)

# Produce a data frame of just what we need in right format for tcp-1234:
df_iperf_1234 <- data.frame()
for (i in 1:length(files_list_1234)) {
    test_type <- unname(files_1234$test_types[i])
    dir_path <- unname(files_1234$dir_path[i])
    df_tmp <- data.frame(i)
    colnames(df_tmp)[1] = "Test Number"
    # Add in the 'y' column(s):
    df_tmp[2] <- files_list_1234[[i]][,5]
    df_tmp[3] <- files_list_1234[[i]][,9]
    colnames(df_tmp)[2] <- "TCP_Port"
    colnames(df_tmp)[3] <- "Bandwidth"
    #*** Add filled Test_Type column:
    df_tmp$Test_Type <- test_type
    #*** Add filled Dir_Path column:
    df_tmp$Dir_Path <- dir_path
    # Accumulate the additional data rows:
    df_iperf_1234 = rbind(df_iperf_1234, df_tmp)
}

# Produce a data frame of just what we need in right format for tcp-5555:
df_iperf_5555 <- data.frame()
for (i in 1:length(files_list_5555)) {
    test_type <- unname(files_5555$test_types[i])
    dir_path <- unname(files_5555$dir_path[i])
    df_tmp <- data.frame(i)
    colnames(df_tmp)[1] = "Test Number"
    # Add in the 'y' column(s):
    df_tmp[2] <- files_list_5555[[i]][,5]
    df_tmp[3] <- files_list_5555[[i]][,9]
    colnames(df_tmp)[2] <- "TCP_Port"
    colnames(df_tmp)[3] <- "Bandwidth"
    #*** Add filled Test_Type column:
    df_tmp$Test_Type <- test_type
    #*** Add filled Dir_Path column:
    df_tmp$Dir_Path <- dir_path
    # Accumulate the additional data rows:
    df_iperf_5555 = rbind(df_iperf_5555, df_tmp)
}

# Produce a merged data frame:
df_combined <-merge(df_iperf_1234, df_iperf_5555, all=T)
# Convert TCP Port to a factor:
df_combined$TCP_Port <- as.factor(df_combined$TCP_Port)

# ============================= CHARTING ===============================
# Plot results on a chart:
g <- ggplot(df_combined, aes(x=TCP_Port, y=Bandwidth)) +
    geom_point(shape=1) +
    facet_grid(. ~ Test_Type, labeller=fx_chart_facet_labeller) +
    scale_y_log10() + ggtitle("Static Regression Tests") +
    xlab("TCP Port") +
    ylab("Bandwidth (bps - Log10)")
print (g)
