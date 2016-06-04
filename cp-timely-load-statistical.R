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
# ============================= CSVs to DATA FRAME =====================
fx_csv2df <- function(files_list, files, col_select, col_names) {
    # Passed a list of CSV file data frames, files information, specific
    # columns of interest and their final names and return a single 
    # data frame that incorporates all this information
    df_result <- data.frame()
    # iterate through files and add to result data frame
    for (i in 1:length(files_list)) {
        test_type <- unname(files$test_types[i])
        dir_path <- unname(files$dir_path[i])

        # TEMP PRINT:
        cat(sprintf("fx_csv2df: test_type=\"%s\" dir_path=\"%s\"\n", test_type, dir_path))
        
        x <- files_list[[i]]$time
        df_tmp <- data.frame(x)
        colnames(df_tmp)[1] = "Time"
        # Add in the 'y' column(s):
        j <- 2
        
        for (col_y in col_select) {

            # TEMP PRINT:
            cat(sprintf("fx_csv2df: col_y=\"%s\" \n", col_y))
        
            df_tmp$col_y <- files_list[[i]][,col_y]
            colnames(df_tmp)[j] <- col_names[j - 1]
            j <- j + 1
        }
        #*** Add filled Test_Type column:
        df_tmp$Test_Type <- as.factor(rep(test_type, length(x)))
        #*** Add filled Dir_Path column:
        df_tmp$Dir_Path <- as.factor(rep(dir_path, length(x)))
        # Accumulate the additional data rows:
        df_result = rbind(df_result, df_tmp)
    }
    # Set Time column to POSIXct data type:
    df_result$Time <- as.POSIXct(df_result$Time)
    return(df_result)
}
# ===================== MAIN PROGRAM ===================================

#*** Timeliness of MAC learning updates applied to data plane
#*** The relies on Open vSwitch snooping, and does not work for nmeta
#*** as it does not apply FEs to data plane when MACs are learnt.

#*** Notes:
#*** nmeta2-active: TBD
#*** nmeta2-passive: TBD
#*** nmeta: TBD
#*** simpleswitch: TBD
#*** nosdn: TBD

# Call function (see further up) to build file data:
files_cp_snoop <- fx_build_file_data("post_process_control_plane_snoop_time_delta.csv", files_dir_2)

print ("Reading result CSV files into a list")
# Read the result CSV files into a list:
files_list_cp_snoop <- lapply(files_cp_snoop$files, read.csv, header=FALSE)

df_cp_snoop <- data.frame()
for (i in 1:length(files_list_cp_snoop)) {
    # Read data frame from list
    df_temp <- files_list_cp_snoop[[i]]
    df_temp$TestType <- unname(files_cp_snoop$dir_path[i])
    names(df_temp) <- c("Test_Type", "Load_Rate", "DP_Apply_Timeliness", "DirPath")
    print ("Doing rbind to accumulate row")
    df_cp_snoop = rbind(df_cp_snoop, df_temp)
}

#---------------- Timeliness of packet flooding ------------------------
#*** Timeliness of packet flooding ceasing (implying that CP has learnt
#*** MAC...

#*** Notes:
#*** nmeta2-active: does not work, as sent to DPAE (???)
#*** nmeta2-passive: does work
#*** nmeta: TBD
#*** simpleswitch: does work
#*** nosdn: TBD

# Call function (see further up) to build file data:
files_cp_traffic <- fx_build_file_data("post_process_control_plane_traffic_time_delta.csv", files_dir_2)

print ("Reading result CSV files into a list")
# Read the result CSV files into a list:
files_list_cp_traffic <- lapply(files_cp_traffic$files, read.csv, header=FALSE)

df_cp_traffic <- data.frame()
for (i in 1:length(files_list_cp_traffic)) {
    # Read data frame from list
    df_temp <- files_list_cp_traffic[[i]]
    df_temp$TestType <- unname(files_cp_traffic$dir_path[i])
    names(df_temp) <- c("Test_Type", "Load_Rate", "No_Flood_Timeliness", "Flooded_Pkts_to_Crafted_MAC", "DirPath")
    print ("Doing rbind to accumulate row")
    df_cp_traffic = rbind(df_cp_traffic, df_temp)
}

#---------------- Packets to/from Controller ------------------------
#*** Telemetry about number of packets to/from the controller
#*** This isn't for inclusion in thesis, just used as sanity check

# Call function (see further up) to build file data:
files_ct_pkts <- fx_build_file_data("post_process_controller_pkts.csv", files_dir_2)

print ("Reading result CSV files into a list")
# Read the result CSV files into a list:
files_list_ct_pkts <- lapply(files_ct_pkts$files, read.csv, header=FALSE)

df_ct_pkts <- data.frame()
for (i in 1:length(files_list_ct_pkts)) {
    # Read data frame from list
    df_temp <- files_list_ct_pkts[[i]]
    df_temp$TestType <- unname(files_ct_pkts$dir_path[i])
    names(df_temp) <- c("Test_Type", "Load_Rate", "Controller_Packets_In", "Controller_Packets_Out", "DirPath")
    print ("Doing rbind to accumulate row")
    df_ct_pkts = rbind(df_ct_pkts, df_temp)
}

#---------------- Packets to/from Server Special NIC ------------------------
#*** Telemetry about number of packets to/from the server special NIC
#*** This isn't for inclusion in thesis, just used as sanity check

# Call function (see further up) to build file data:
files_sv_pkts <- fx_build_file_data("post_process_server_pkts.csv", files_dir_2)

print ("Reading result CSV files into a list")
# Read the result CSV files into a list:
files_list_sv_pkts <- lapply(files_sv_pkts$files, read.csv, header=FALSE)

df_sv_pkts <- data.frame()
for (i in 1:length(files_list_sv_pkts)) {
    # Read data frame from list
    df_temp <- files_list_sv_pkts[[i]]
    df_temp$TestType <- unname(files_sv_pkts$dir_path[i])
    names(df_temp) <- c("Test_Type", "Load_Rate", "Server_Packets_In", "Server_Packets_Out", "DirPath")
    print ("Doing rbind to accumulate row")
    df_sv_pkts = rbind(df_sv_pkts, df_temp)
}


#-------------------- Switch OS Analysis -------------------------------
# Call function (see further up) to build file data:
files <- fx_build_file_data("sw1.example.com-mosp.csv", files_dir_2)

print ("Reading Switch mosp result CSV files into a list")
# Read the result CSV files into a list:
files_list <- lapply(files$files, read.csv)

print ("Generating Switch mosp data frame")
col_select <- c("sw1.cpu.0.", "sw1.swap.in", "sw1.swap.out")
col_names <- c("Switch_CPU", "Switch_Swap_In", "Switch_Swap_Out")
df_sw_mosp <- fx_csv2df(files_list, files, col_select, col_names)

# ======================== CHARTING ====================================

# Data Plane update delay for Learnt MAC by Test Type
q <- ggplot(data=df_cp_snoop, aes(x=Load_Rate, y=DP_Apply_Timeliness, fill=Test_Type, color=Test_Type)) + xlab("NFPS Load") + ylab("MAC Learning Treatment Delay (s)") + theme(legend.title=element_blank()) + geom_point(aes(x=Load_Rate, y=DP_Apply_Timeliness, color=Test_Type)) + stat_smooth(method = "loess") + theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))
print (q)

# Learning delay to Not Flood Crafted MAC by Test Type
q <- ggplot(data=df_cp_traffic, aes(x=Load_Rate, y=No_Flood_Timeliness, fill=Test_Type, color=Test_Type)) + xlab("NFPS Load") + ylab("MAC Learning No Flooding Delay (s)") + theme(legend.title=element_blank()) + geom_point(aes(x=Load_Rate, y=No_Flood_Timeliness, color=Test_Type)) + stat_smooth(method = "loess") + theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))
print (q)

# Packets Flooded to Crafted MAC by Test Type
q <- ggplot(data=df_cp_traffic, aes(x=Load_Rate, y=Flooded_Pkts_to_Crafted_MAC, fill=Test_Type, color=Test_Type)) + xlab("NFPS Load") + ylab("Number of Packets Flooded to Crafted MAC (packets)") + theme(legend.title=element_blank()) + geom_point(aes(x=Load_Rate, y=Flooded_Pkts_to_Crafted_MAC, color=Test_Type)) + stat_smooth(method = "loess") + theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))
print (q)

# Packets to Controller
q <- ggplot(data=df_ct_pkts, aes(x=Load_Rate, y=Controller_Packets_In, fill=Test_Type, color=Test_Type)) + xlab("NFPS Load") + ylab("Packets to Controller over Test Period for Telemetry (packets)") + theme(legend.title=element_blank()) + geom_point(aes(x=Load_Rate, y=Controller_Packets_In, color=Test_Type)) + stat_smooth(method = "loess") + theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))
print (q)

# Packets from Controller
q <- ggplot(data=df_ct_pkts, aes(x=Load_Rate, y=Controller_Packets_Out, fill=Test_Type, color=Test_Type)) + xlab("NFPS Load") + ylab("Packets from Controller over Test Period for Telemetry (packets)") + theme(legend.title=element_blank()) + geom_point(aes(x=Load_Rate, y=Controller_Packets_Out, color=Test_Type)) + stat_smooth(method = "loess") + theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))
print (q)

# Packets to Server
q <- ggplot(data=df_sv_pkts, aes(x=Load_Rate, y=Server_Packets_In, fill=Test_Type, color=Test_Type)) + xlab("NFPS Load") + ylab("Packets to Server over Test Period for Telemetry (packets)") + theme(legend.title=element_blank()) + geom_point(aes(x=Load_Rate, y=Server_Packets_In, color=Test_Type)) + stat_smooth(method = "loess") + theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))
print (q)

# Packets from Server
q <- ggplot(data=df_sv_pkts, aes(x=Load_Rate, y=Server_Packets_Out, fill=Test_Type, color=Test_Type)) + xlab("NFPS Load") + ylab("Packets from Server over Test Period for Telemetry (packets)") + theme(legend.title=element_blank()) + geom_point(aes(x=Load_Rate, y=Server_Packets_Out, color=Test_Type)) + stat_smooth(method = "loess") + theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))
print (q)
