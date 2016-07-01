# Analyse results from a timeliness no-load statistical test

# Run in rstudio with:

# source("~/R_analysis/tc-timely-noload-statistical.R")

# Imports:
libs <- c('ggplot2', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales', 'zoo', 'scales')
lapply(libs, require, character.only = T)

# Base directory where results data is stored:
base_dir <- "~/results/timeliness/statistical"

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
fx_build_file_data <- function(file_name, files_dir_2, bad_tests) {
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
                if (!(test_timestamp %in% bad_tests)) {
                    full_path = paste(base_dir_4, file_name, sep = '/')
                    # Append the full path of the file to the list
                    files_vector <- c(files_vector, full_path)
                    # Use test_types to hold mapping between full file path and type of test:
                    test_types_vector[full_path] <- test_type
                    # Store the directory path:
                    dir_path_vector[full_path] <- base_dir_4
                }
                else {
                    cat(sprintf("Warning, ignoring bad test type=\"%s\" timestamp=\"%s\" because of errors. Please fix...\n", test_type, test_timestamp))
                }
            }
        }
    }
    returnList <- list("files" = files_vector,
                       "test_types" = test_types_vector,
                       "dir_path" = dir_path_vector)
    return(returnList)
}

# =============================== BUILD FILE DATA FUNCTION =============
fx_check_4_bad_tests <- function(files_dir_2) {
    # Use this to build file data of directories that should be excluded
    # because they have are bad tests (i.e. something went wrong, so
    # their presence should be recorded but excluded from results)
    bad_test_timestamps <- vector()

    for (test_type in files_dir_2) {
        base_dir_3 <- paste(base_dir_2, test_type, sep = '/')
        files_dir_3 <- list.files(path=base_dir_3)
        for (test_timestamp in files_dir_3) {
            base_dir_4 <- paste(base_dir_3, test_timestamp, sep = '/')
            files_dir_4 <- list.files(path=base_dir_4)
            # Does error.txt exist in this directory?:
            if (is.element("error.txt", files_dir_4)) {
                # Append the bad test timestamp to a vector:
                bad_test_timestamps <- c(bad_test_timestamps, test_timestamp)
            }
        }
    }
    return(bad_test_timestamps)
}

# ===================== MAIN PROGRAM ===================================

# Remove any bad test runs as they can corrupt the results in strange
# ways but ensure that the presence of bad test runs is made visible
# as this indicates a problem that needs to be fixed...
bad_test_timestamps = fx_check_4_bad_tests(files_dir_2)

#========================= Iperf Results ===============================
# Note: Iperf CSV format is:
# Time, local_IP, local_port, server_IP, server_port, duration,
#   Interval, Transfer, Bandwidth

# Call function (see further up) to build file data:
files_pc <- fx_build_file_data("pc1.example.com-5555-iperf_result.txt", files_dir_2, bad_test_timestamps)

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

#======================== Treatment Time ===============================
# Call function (see further up) to build file data:
files_tt <- fx_build_file_data("post_process_treatment_time_delta.txt", files_dir_2, bad_test_timestamps)

print ("Reading Treatment Time result CSV files into a list")
# Read the result CSV files into a list:
files_list_tt <- lapply(files_tt$files, read.csv, header=FALSE)

# Produce a data frame of treatment time:
df_tt <- data.frame()
for (i in 1:length(files_list_tt)) {
    test_type <- unname(files_tt$test_types[i])
    dir_path <- unname(files_tt$dir_path[i])
    df_tmp <- data.frame(i)
    colnames(df_tmp)[1] = "Test Number"
    # Add in the 'y' column(s):
    df_tmp[2] <- files_list_tt[[i]][,1]
    colnames(df_tmp)[2] <- "Time_to_Treatment"
    #*** Add filled Test_Type column:
    df_tmp$Test_Type <- test_type
    #*** Add filled Dir_Path column:
    df_tmp$Dir_Path <- dir_path
    # Accumulate the additional data rows:
    df_tt = rbind(df_tt, df_tmp)
}

#======================== Packets to DPAE ==============================
# Call function (see further up) to build file data:
files_p2dpae <- fx_build_file_data("post_process_dpae_pkts.txt", files_dir_2, bad_test_timestamps)

print ("Reading Packets to DPAE result CSV files into a list")
# Read the result CSV files into a list:
files_list_p2dpae <- lapply(files_p2dpae$files, read.csv, header=FALSE)

# Produce a data frame of packets to DPAE:
df_p2dpae <- data.frame()
for (i in 1:length(files_list_p2dpae)) {
    test_type <- unname(files_p2dpae$test_types[i])
    dir_path <- unname(files_p2dpae$dir_path[i])
    df_tmp <- data.frame(i)
    colnames(df_tmp)[1] = "Test Number"
    # Add in the 'y' column(s):
    df_tmp[2] <- files_list_p2dpae[[i]][,1]
    colnames(df_tmp)[2] <- "Packets_to_DPAE"
    #*** Add filled Test_Type column:
    df_tmp$Test_Type <- test_type
    #*** Add filled Dir_Path column:
    df_tmp$Dir_Path <- dir_path
    # Accumulate the additional data rows:
    df_p2dpae = rbind(df_p2dpae, df_tmp)
}

#======================== Combined DF =========================
df_combined <- merge(df_tt, df_p2dpae,by="Test Number")
# Fix double-up of columns:
drops <- c("Test_Type.y","Dir_Path.y")
df_combined <- df_combined[,!(names(df_combined) %in% drops)]
colnames(df_combined)[names(df_combined)=="Test_Type.x"] <- "Test_Type"
colnames(df_combined)[names(df_combined)=="Dir_Path.x"] <- "Dir_Path"

df_combined <- merge(df_combined, df_iperf_pc,by="Test Number")
# Fix double-up of columns:
drops <- c("Test_Type.y","Dir_Path.y")
df_combined <- df_combined[,!(names(df_combined) %in% drops)]
colnames(df_combined)[names(df_combined)=="Test_Type.x"] <- "Test_Type"
colnames(df_combined)[names(df_combined)=="Dir_Path.x"] <- "Dir_Path"

# ============================= CHARTING ===============================

# Packets to DPAE vs Time to Treat:
print("Creating chart for Packets to DPAE vs Time to Treat")
q <- qplot(df_combined$"Time_to_Treatment", df_combined$"Packets_to_DPAE", color=df_combined$"Test_Type", main="Packets to DPAE vs Time to Treat", xlab="Time to Treatment (seconds)", ylab="Packets to DPAE (packets)")
q + labs(color="custom title")
print (q)

# Bandwidth vs Time to Treatment:
print("Creating chart for Bandwidth vs Time to Treatment")
q <- qplot(df_combined$"Time_to_Treatment", df_combined$"Bandwidth", color=df_combined$"Test_Type", main="Bandwidth vs Time to Treatment", xlab="Time to Treatment (seconds)", ylab="Bandwidth (bps)")
print (q)

# Bandwidth vs Packets to DPAE:
print("Creating chart for Bandwidth vs Packets to DPAE")
q <- qplot(df_combined$"Packets_to_DPAE", df_combined$"Bandwidth", color=df_combined$"Test_Type", main="Nmeta2 Statistical Classifier - Iperf Average Bandwidth vs TC Packets to DPAE by Mode", xlab="Packets to DPAE (log10 scale)", ylab="Bandwidth (bps -  log10 scale)") + scale_x_log10(limits=c(1, 1000), breaks=c(0.1, 1, 3, 10, 33, 100, 333), labels = comma) + scale_y_log10(limits=c(50000, 1000000), breaks=c(100000, 333333, 1000000),labels = comma) + geom_point(aes(shape=df_combined$"Test_Type"), size = 3) + theme(legend.title=element_blank())
print (q)

# Bandwidth vs Packets to DPAE 2:
print("Creating chart for Bandwidth vs Packets to DPAE - for Publishing Paper half page width to be readable")
q <- ggplot() + xlab("\nPackets to DPAE (log10 scale)") + ylab("Bandwidth (bps -  log10 scale)\n") + theme(legend.title=element_blank()) + scale_x_log10(limits=c(1, 1000), breaks=c(0.1, 1, 3, 10, 33, 100, 333), labels = comma) + scale_y_log10(limits=c(50000, 1000000), breaks=c(100000, 333333, 1000000),labels = comma) + geom_point(data = df_combined, aes(x = Packets_to_DPAE, y = Bandwidth, color = df_combined$"Test_Type")) + theme(axis.title.x = element_text(size=15), axis.title.y = element_text(size=15)) + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) + theme(legend.position = c(.3, .7)) + theme(legend.text=element_text(size=12))
print (q)
