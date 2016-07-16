# Analyse results from a multi-switch performance no-load test

# Run in rstudio with:

# source("~/R_analysis/multi-switch-performance-no-load-analysis.R")

# Imports:
libs <- c('ggplot2', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales', 'zoo', 'scales')
lapply(libs, require, character.only = T)

# Base directory where results data is stored:
base_dir <- "~/results/multi-switch-performance-no-load"

# Ask for the sub-directory that the results are in:
test_dir_1 <- readline("What is name of directory?")

base_dir_2 <- paste(base_dir, test_dir_1, sep = '/')

# EXAMPLE FULL DIR:
#~/results/multi-switch-performance-no-load/20160716203320/1/nmeta2-active/20160716203528

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
    # the directory structure and return list of 4 vectors that are needed
    # Note that this is a special version for multi-switch directories
    # Example: multi-switch-performance-no-load/20160716210139/1/nosdn/20160716210654
    files_vector <- vector()
    switches_vector <- vector()
    test_types_vector <- vector()
    dir_path_vector <- vector()

    for (switches in files_dir_2) {
        base_dir_3 <- paste(base_dir_2, switches, sep = '/')
        files_dir_3 <- list.files(path=base_dir_3)
        for (test_type in files_dir_3) {
            base_dir_4 <- paste(base_dir_3, test_type, sep = '/')
            files_dir_4 <- list.files(path=base_dir_4)
            for (test_timestamp in files_dir_4) {
                base_dir_5 <- paste(base_dir_4, test_timestamp, sep = '/')
                files_dir_5 <- list.files(path=base_dir_5)
                if (is.element(file_name, files_dir_5)) {
                    full_path = paste(base_dir_5, file_name, sep = '/')
                    # Append the full path of the file to the list
                    files_vector <- c(files_vector, full_path)
                    # Number of switches in path:
                    switches_vector[full_path] <- switches
                    # Use test_types to hold mapping between full file path and type of test:
                    test_types_vector[full_path] <- test_type
                    # Store the directory path:
                    dir_path_vector[full_path] <- base_dir_5
                }
            }
        }
    }
    returnList <- list("files" = files_vector,
                       "switches" = switches_vector,
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
        switches <- unname(files$switches[i])
        test_type <- unname(files$test_types[i])
        dir_path <- unname(files$dir_path[i])

        # TEMP PRINT:
        cat(sprintf("fx_csv2df: switches=\"%s\" test_type=\"%s\" dir_path=\"%s\"\n", switches, test_type, dir_path))
        
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
        #*** Add filled Switches column:
        df_tmp$Switches <- as.factor(rep(switches, length(x)))
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

# ============================= CSVs to DATA FRAME =====================
fx_column2df <- function(files_list, files, col_name) {
    # Passed a list of CSV (single column, no header) file data frames,
    # files information, specific columns of interest and their final
    # names and return a single data frame that incorporates all this
    # information
    df_result <- data.frame()
    # iterate through files and add to result data frame
    for (i in 1:length(files_list)) {
        test_type <- unname(files$test_types[i])
        dir_path <- unname(files$dir_path[i])

        # TEMP PRINT:
        cat(sprintf("fx_csv2df: test_type=\"%s\" dir_path=\"%s\"\n", test_type, dir_path))
        
        x <- files_list[[i]][1]

        # TEMP PRINT:
        cat(sprintf("x=\"%s\" \"\n", x))

        df_tmp <- data.frame(x)
        colnames(df_tmp)[1] = col_name
        #*** Add filled Test_Type column:
        df_tmp$Test_Type <- as.factor(rep(test_type, length(x)))
        #*** Add filled Dir_Path column:
        df_tmp$Dir_Path <- as.factor(rep(dir_path, length(x)))
        # Accumulate the additional data rows:
        df_result = rbind(df_result, df_tmp)
    }
    return(df_result)
}

# ===================== MAIN PROGRAM ===================================

# ===================== hort client cxn-close analysis:
# Call function (see further up) to build file data:
files <- fx_build_file_data("pc1.example.com-hort-cxn-close.csv", files_dir_2)

print ("Reading hort client cxn-close result CSV files into a list")
# Read the result CSV files into a list:
files_list <- lapply(files$files, read.csv)

print ("Generating hort client cxn-close data frame")
col_select <- c("pc1.cxn.close.retrieval.time")
col_names <- c("Object_Retrieval_Time")
df_cxn_close <- fx_csv2df(files_list, files, col_select, col_names)

#*** Create a statistical analysis for hort client cxn-close data:
df_cxn_close_stats <- ddply(df_cxn_close, c("Test_Type"), summarise,
               N    = length(Object_Retrieval_Time),
               mean = mean(Object_Retrieval_Time),
               sd   = sd(Object_Retrieval_Time),
               se   = sd / sqrt(N)
)

# ===================== hping3 client analysis:
# Call function (see further up) to build file data:
files <- fx_build_file_data("post_process_hping3.csv", files_dir_2)

print ("Reading hping3 client result CSV files into a list")
# Read the result CSV files into a list:
files_list <- lapply(files$files, read.csv)

print ("Generating hping3 client data frame")
col_name <- c("tcp_rtt")
df_hping3 <- fx_column2df(files_list, files, col_name)

#*** Create a statistical analysis for hping3 client result data:
df_hping3_stats <- ddply(df_hping3, c("Test_Type"), summarise,
               N    = length(tcp_rtt),
               mean = mean(tcp_rtt),
               sd   = sd(tcp_rtt),
               se   = sd / sqrt(N)
)

# ============================= CHARTING ===============================
# Plot results on a chart:
# Packets to DPAE vs Time to Treat 2:
print("Creating chart for Object Retrieval Time by Test Type")
q <- qplot(df_cxn_close$"Test_Type", df_cxn_close$"Object_Retrieval_Time", main="Object Retrieval Time by Test Type", xlab="Test Type", ylab="Object_Retrieval_Time (seconds)")
q + labs(color="custom title")
print (q)

print("Creating bar chart for hort cxn close object retrieval time")
q <- ggplot(data=df_cxn_close_stats, aes(x=Test_Type, y=mean, fill=Test_Type)) + geom_bar(stat="identity") + ylab("HTTP Object Retrieval Time (s)") + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9))
print (q)

print("Creating bar chart for TCP RTT (as measured by hping3)")
q <- ggplot(data=df_hping3_stats, aes(x=Test_Type, y=mean, fill=Test_Type)) + ylab("TCP RTT (s)") + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9))
print (q)

print("Creating bar chart for TCP RTT (as measured by hping3) - for Publishing Paper half page width to be readable")
q <- ggplot(data=df_hping3_stats, aes(x=Test_Type, y=mean, fill=Test_Type)) + xlab("\nTest Type") + ylab("TCP RTT (s)\n") + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
    theme(axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), axis.text.x = element_text(size=15), axis.text.y = element_text(size=15), legend.position="none")
print (q)
