# Analyse results from a new flow per second (NFPS) load test

# Version 0.2.1

# Run in rstudio with:

# source("~/R_analysis/nmeta2-nfps-load-test-analysis.R")

# Imports:
libs <- c('ggplot2', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales', 'zoo', 'scales', 'cowplot')
lapply(libs, require, character.only = T)

# Base directory where results data is stored:
base_dir <- "~/results/nfps-load-tests/nmeta2-combined/"

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

# ===================== CHART FUNCTIONS ================================
fx_chart_scatter_1 <- function(data_x, data_y, data_type, data.frame, chart_title, x_axis_label, y_axis_label) {
    # Scatter lattice with panel per test type and R squared stat analysis:
    # Build formula from variables we were passed:
    y_vs_x <- paste(data_y, data_x, sep="~")
    chart_formula <- formula(paste(y_vs_x, data_type, sep="|"))
    scatter.lattice <- xyplot(chart_formula, 
                          data = data.frame,
                          main=chart_title,
                          panel = function(x, y, ...) {
                            panel.xyplot(x, y, ...)
                            lm1 <- lm(y ~ x)
                            lm1sum <- summary(lm1)
                            r2 <- lm1sum$adj.r.squared
                            panel.abline(a = lm1$coefficients[1], 
                                         b = lm1$coefficients[2])
                            panel.text(labels = 
                                         bquote(italic(R)^2 == 
                                                  .(format(r2, 
                                                           digits = 3))),
                                       x = 30, y = 1000)
                            },
                          xscale.components = xscale.components.subticks,
                          yscale.components = yscale.components.subticks,
                          xlab=x_axis_label,
                          ylab=y_axis_label,
                          as.table = TRUE)
    p = scatter.lattice
    print (p)
}
# ===================== DATA MANIPULATION FUNCTIONS ====================
fx_index_by_load <- function(df_results, df_filt) {
    # Create a data frame that is indexed against the filt NFPS load
    # Use merge to create a combined result/filt data frame:
    print("fx_index_by_load: Doing initial result df/filt df merge...")
    df_1 <-merge(df_results, df_filt, all=T, by="Time")
    df_combined = data.frame()

    # Remove leading rows with NA for filt_Actual_Rate as they precede the
    #  start of the test, for each test (use Dir_Path to cut per test run):
    for (Dir_Path in unique(df_1$Dir_Path.x)) {
        if(!is.na(Dir_Path)) {
            # Subset for particular test run results and filt:
            df_2 <- df_1[ which((df_1$Dir_Path.x == Dir_Path) | (df_1$Dir_Path.y == Dir_Path)), ]
            # First row that filt results start in:
            first_row <- which.min(is.na(df_2$Previous_Actual_Rate))
            # Trim rows before it off the df:
            df_2 <- df_2[-(1:(first_row-1)),]

            # Use zoo package na.locf to fill Previous_Actual_Rate from next value present:
            df_2$Previous_Actual_Rate <- na.locf(df_2$Previous_Actual_Rate, fromLast = TRUE, na.rm = FALSE)

            # Accumulate the additional data rows:
            df_combined = rbind(df_combined, df_2)
        }
    }

    print("fx_index_by_load: remove NAs")
    # subset df with only rows that have complete data in column 2:
    df_combined <- df_combined[complete.cases(df_combined[, 2]),]
    # subset df with only rows that have complete data for filt Previous_Actual_Rate:
    df_combined <- df_combined[complete.cases(df_combined[, "Previous_Actual_Rate"]),]

    print("fx_index_by_load: remove superfluous columns and tidy up names")
    drops <- c("Test_Type.y","Dir_Path.y")
    df_combined <- df_combined[,!(names(df_combined) %in% drops)]
    colnames(df_combined)[names(df_combined)=="Test_Type.x"] <- "Test_Type"
    colnames(df_combined)[names(df_combined)=="Dir_Path.x"] <- "Dir_Path"
    colnames(df_combined)[names(df_combined)=="Previous_Actual_Rate"] <- "Load_Rate"
    return(df_combined)
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
      file_match <- grep("lg1\\.example\\.com\\-filt\\-.*.csv", file_name)
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
# Set Time column to POSIXct data type:
df_filt$Time <- as.POSIXct(df_filt$Time)

# ===================== Controller OS analysis:
# Call function (see further up) to build file data:
files <- fx_build_file_data("ct1.example.com-mosp.csv", files_dir_2)

print ("Reading Controller mosp result CSV files into a list")
# Read the result CSV files into a list:
files_list <- lapply(files$files, read.csv)

print ("Generating Controller mosp data frame")
col_select <- c("ct1.cpu.0.", "ct1.swap.in", "ct1.swap.out", "ct1.pkts.in.eth2.", "ct1.pkts.out.eth2.")
col_names <- c("Controller_CPU", "Controller_Swap_In", "Controller_Swap_Out", "Controller_Pkt_In", "Controller_Pkt_Out")
df_ct_mosp <- fx_csv2df(files_list, files, col_select, col_names)

# Create a data frame that is indexed by filt NFPS load:
df_ct_mosp_filt = fx_index_by_load(df_ct_mosp, df_filt)

# ===================== Switch OS analysis:
# Call function (see further up) to build file data:
files <- fx_build_file_data("sw1.example.com-mosp.csv", files_dir_2)

print ("Reading Switch mosp result CSV files into a list")
# Read the result CSV files into a list:
files_list <- lapply(files$files, read.csv)

print ("Generating Switch mosp data frame")
col_select <- c("sw1.cpu.0.", "sw1.swap.in", "sw1.swap.out")
col_names <- c("Switch_CPU", "Switch_Swap_In", "Switch_Swap_Out")
df_sw_mosp <- fx_csv2df(files_list, files, col_select, col_names)

# Create a data frame that is indexed by filt NFPS load:
df_sw_mosp_filt = fx_index_by_load(df_sw_mosp, df_filt)

# ===================== DPAE OS analysis:
# Call function (see further up) to build file data:
files <- fx_build_file_data("dp1.example.com-mosp.csv", files_dir_2)

print ("Reading DPAE mosp result CSV files into a list")
# Read the result CSV files into a list:
files_list <- lapply(files$files, read.csv)

print ("Generating DPAE mosp data frame")
# HASH OUT ONE OF THESE, FIRST IS FOR BACKWARD COMPATIBILITY WITH OLD RESULTS:
col_select <- c("dp1.cpu", "dp1.swap.in", "dp1.swap.out", "dp1.pkt.in", "dp1.pkt.out")
#col_select <- c("dp1.cpu.0.", "dp1.swap.in", "dp1.swap.out", "dp1.pkts.in.eth1.", "dp1.pkts.in.eth1.")
col_names <- c("DPAE_CPU", "DPAE_Swap_In", "DPAE_Swap_Out", "DPAE_Pkt_In", "DPAE_Pkt_Out")
df_dp_mosp <- fx_csv2df(files_list, files, col_select, col_names)

# Create a data frame that is indexed by filt NFPS load:
df_dp_mosp_filt = fx_index_by_load(df_dp_mosp, df_filt)

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

# Create a data frame that is indexed by filt NFPS load:
df_cxn_close_filt = fx_index_by_load(df_cxn_close, df_filt)

# ===================== hort client cxn-keepalive analysis:
# Call function (see further up) to build file data:
files <- fx_build_file_data("pc1.example.com-hort-cxn-keepalive.csv", files_dir_2)

print ("Reading hort client cxn-keepalive result CSV files into a list")
# Read the result CSV files into a list:
files_list <- lapply(files$files, read.csv)

print ("Generating hort client cxn-keepalive data frame")
col_select <- c("pc1.cxn.keep.alive.retrieval.time")
col_names <- c("Object_Retrieval_Time")
df_cxn_keepalive <- fx_csv2df(files_list, files, col_select, col_names)

# Create a data frame that is indexed by filt NFPS load:
df_cxn_keepalive_filt = fx_index_by_load(df_cxn_keepalive, df_filt)

# ========== DELETED NMETA EVENTRATE AND PACKETTIME FROM HERE ==========

# ============================= CHARTING ===============================

# Call our function to create charts (see top of this program)

# Cxn-close chart:
print("Client cxn-close: creating chart")
fx_chart_scatter_1("Load_Rate", "Object_Retrieval_Time", "Test_Type", df_cxn_close_filt, "Connection Close Retrieval Time vs New Flows Load by Test Type", "Load Rate (NFPS)", "Object Retrieval Time (seconds)")

# Cxn-keepalive chart:
print("Client cxn-keepalive: creating chart")
fx_chart_scatter_1("Load_Rate", "Object_Retrieval_Time", "Test_Type", df_cxn_keepalive_filt, "Connection Keepalive Retrieval Time vs New Flows Load by Test Type", "Load Rate", "Object Retrieval Time (seconds)")

# Controller CPU:
print("Controller mosp: creating CPU chart")
fx_chart_scatter_1("Load_Rate", "Controller_CPU", "Test_Type", df_ct_mosp_filt, "Controller CPU vs New Flows Load by Test Type", "Load Rate", "CPU Load (%)")

# Controller Swap In:
print("Controller mosp: creating Swap In chart")
fx_chart_scatter_1("Load_Rate", "Controller_Swap_In", "Test_Type", df_ct_mosp_filt, "Controller Swap In vs New Flows Load by Test Type", "Load Rate", "Swap In (Bytes) per interval")

# Controller Swap Out:
print("Controller mosp: creating Swap Out chart")
fx_chart_scatter_1("Load_Rate", "Controller_Swap_Out", "Test_Type", df_ct_mosp_filt, "Controller Swap Out vs New Flows Load by Test Type", "Load Rate", "Swap Out (Bytes) per interval")

# Controller Ethernet Packets In:
print("Controller mosp: creating Packets In chart")
fx_chart_scatter_1("Load_Rate", "Controller_Pkt_In", "Test_Type", df_ct_mosp_filt, "Controller Ethernet Packets In vs New Flows Load by Test Type", "Load Rate", "Packets Received per Interval")

# Switch CPU:
print("Switch mosp: creating CPU chart")
fx_chart_scatter_1("Load_Rate", "Switch_CPU", "Test_Type", df_sw_mosp_filt, "Switch CPU vs New Flows Load by Test Type", "Load Rate", "Switch CPU (%)")

# Switch Swap In:
print("Switch mosp: creating Swap In chart")
fx_chart_scatter_1("Load_Rate", "Switch_Swap_In", "Test_Type", df_sw_mosp_filt, "Switch Swap In vs New Flows Load by Test Type", "Load Rate", "Swap In (Bytes) per interval")

# Switch Swap Out:
print("Switch mosp: creating Swap Out chart")
fx_chart_scatter_1("Load_Rate", "Switch_Swap_Out", "Test_Type", df_sw_mosp_filt, "Switch Swap Out vs New Flows Load by Test Type", "Load Rate", "Swap Out (Bytes) per interval")

# DPAE CPU:
print("DPAE mosp: creating CPU chart")
fx_chart_scatter_1("Load_Rate", "DPAE_CPU", "Test_Type", df_dp_mosp_filt, "DPAE CPU vs New Flows Load by Test Type", "Load Rate", "DPAE CPU (%)")

# DPAE Swap In:
print("DPAE mosp: creating Swap In chart")
fx_chart_scatter_1("Load_Rate", "DPAE_Swap_In", "Test_Type", df_dp_mosp_filt, "DPAE Swap In vs New Flows Load by Test Type", "Load Rate", "Swap In (Bytes) per interval")

# DPAE Swap Out:
print("DPAE mosp: creating Swap Out chart")
fx_chart_scatter_1("Load_Rate", "DPAE_Swap_Out", "Test_Type", df_dp_mosp_filt, "DPAE Swap Out vs New Flows Load by Test Type", "Load Rate", "Swap Out (Bytes) per interval")

# DPAE Ethernet Packets In:
print("DPAE mosp: creating Packets In chart")
fx_chart_scatter_1("Load_Rate", "DPAE_Pkt_In", "Test_Type", df_dp_mosp_filt, "DPAE Ethernet Packets In vs New Flows Load by Test Type", "Load Rate", "Packets Received per Interval")

# DPAE Ethernet Packets Out:
print("DPAE mosp: creating Packets Out chart")
fx_chart_scatter_1("Load_Rate", "DPAE_Pkt_Out", "Test_Type", df_dp_mosp_filt, "DPAE Ethernet Packets Out vs New Flows Load by Test Type", "Load Rate", "Packets Sent per Interval")

#==================== NEW CHARTS:

#*** This helps get font size bigger:
theme_set(theme_gray(base_size = 18))

# Cxn-close chart:
print("Client cxn-close: creating chart")
q <- qplot(df_cxn_close_filt$"Load_Rate", df_cxn_close_filt$"Object_Retrieval_Time", color=df_cxn_close_filt$"Test_Type", xlab="Load Rate (NFPS)", ylab="Connection Close HTTP Object Retrieval Time (s, log10 scale)") + scale_x_continuous(limits=c(0, 1000), breaks=c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) + scale_y_log10(limits=c(0.001, 10), breaks=c(0.001, 0.01, 0.1, 1, 10), labels = comma) + stat_smooth(method = "loess", formula = y ~ x, size = 1) + theme(legend.title=element_blank()) + theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))
print (q)

# Cxn-close chart (for publishing paper with bigger fonts and legend over plot area to save space):
print("Client cxn-close: creating chart 2")
q <- qplot(df_cxn_close_filt$"Load_Rate", df_cxn_close_filt$"Object_Retrieval_Time", color=df_cxn_close_filt$"Test_Type", xlab="\nLoad Rate (NFPS)", ylab="Connection Close HTTP Object Retrieval Time (s, log10 scale)\n") + scale_x_continuous(limits=c(0, 1000), breaks=c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) + scale_y_log10(limits=c(0.001, 10), breaks=c(0.001, 0.01, 0.1, 1, 10), labels = comma) + stat_smooth(method = "loess", formula = y ~ x, size = 1) + theme(legend.title=element_blank()) + theme(axis.title.x = element_text(size=15), axis.title.y = element_text(size=15), axis.text.x = element_text(size=14), axis.text.y = element_text(size=14), legend.position = c(.87, .6))
print (q)

# Cxn-keepalive chart:
print("Client cxn-keepalive: creating chart")
q <- qplot(df_cxn_keepalive_filt$"Load_Rate", df_cxn_keepalive_filt$"Object_Retrieval_Time", color=df_cxn_keepalive_filt$"Test_Type", xlab="Load Rate (NFPS)", ylab="Connection Keepalive HTTP Object Retrieval Time (s, log10 scale)") + scale_x_continuous(limits=c(0, 1000), breaks=c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) + scale_y_log10(limits=c(0.001, 10), breaks=c(0.001, 0.01, 0.1, 1, 10), labels = comma) + stat_smooth(method = "loess", formula = y ~ x, size = 1) + theme(legend.title=element_blank()) + theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))
print (q)

# Cxn-keepalive chart (for publishing paper with bigger fonts and legend over plot area to save space):
print("Client cxn-keepalive: creating chart 2")
q <- qplot(df_cxn_keepalive_filt$"Load_Rate", df_cxn_keepalive_filt$"Object_Retrieval_Time", color=df_cxn_keepalive_filt$"Test_Type", xlab="\nLoad Rate (NFPS)", ylab="Connection Keepalive HTTP Object Retrieval Time (s, log10 scale)\n") + scale_x_continuous(limits=c(0, 1000), breaks=c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) + scale_y_log10(limits=c(0.001, 1), breaks=c(0.001, 0.01, 0.1, 1), labels = comma) + stat_smooth(method = "loess", formula = y ~ x, size = 1) + theme(legend.title=element_blank()) + theme(axis.title.x = element_text(size=15), axis.title.y = element_text(size=15), axis.text.x = element_text(size=14), axis.text.y = element_text(size=14), legend.position = c(.16, .74))
print (q)

q <- ggplot(data=df_cxn_keepalive_filt, aes(x=Load_Rate, y=Object_Retrieval_Time, fill=Test_Type, color=Test_Type)) + xlab("\nLoad Rate (NFPS)") + ylab("Connection Keepalive HTTP Object Retrieval Time (s, log10 scale)\n") + theme(legend.title=element_blank()) + scale_x_continuous(limits=c(0, 1000), breaks=c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) + scale_y_log10(limits=c(0.001, 1), breaks=c(0.001, 0.01, 0.1, 1), labels = comma) + stat_smooth(method = "loess") + theme(axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.position = c(.16, .74))
print (q)

# Packets to Controller by Test Type
q <- ggplot(data=df_ct_mosp_filt, aes(x=Load_Rate, y=Controller_Pkt_In, fill=Test_Type, color=Test_Type)) + xlab("NFPS Load") + ylab("Controller Packets Received per Interval (pkts, log10 scale)") + theme(legend.title=element_blank()) + scale_x_continuous(limits=c(10, 1000)) + geom_point(aes(x=Load_Rate, y=Controller_Pkt_In, color=Test_Type)) + scale_y_log10(limits=c(0.1, 1000), breaks=c(1, 3, 10, 33, 100, 333, 1000), labels = comma) + stat_smooth(method = "loess") + theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))
print (q)


#===================== CHARTS FOR NMETA (classic) Analysis
# Create an nmeta cxn-close (Drop nmeta2 factors from DF):
factor_to_remove <- which(df_cxn_close_filt$Test_Type=="nmeta2-active")
df_cxn_close_filt2 <- df_cxn_close_filt[-factor_to_remove,]
factor_to_remove <- which(df_cxn_close_filt2$Test_Type=="nmeta2-passive")
df_cxn_close_filt2 <- df_cxn_close_filt2[-factor_to_remove,]
df_cxn_close_filt2$Test_Type <- factor(df_cxn_close_filt2$Test_Type)
q1 <- ggplot(data=df_cxn_close_filt2, aes(x=Load_Rate, y=Object_Retrieval_Time, fill=Test_Type, color=Test_Type)) + xlab("NFPS Load") + ylab("Connection Close HTTP Object Retrieval Time (s)") + theme(legend.title=element_blank()) + scale_x_continuous(limits=c(10, 200)) + geom_point(aes(x=Load_Rate, y=Object_Retrieval_Time, color=Test_Type)) + stat_smooth(method = "loess") + scale_y_continuous(limits=c(0.001, 4), breaks=c(0.5,1,1.5,2,2.5,3,3.5,4), labels = comma) + theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))
#print (q1)

# Convert type on df column to num so that smooth line works (won't work for integer)
df_ct_mosp_filt <- transform(df_ct_mosp_filt, Controller_Pkt_In = as.numeric(Controller_Pkt_In))

# Create an nmeta controller pkt in (Drop nmeta2 factors from DF):
factor_to_remove <- which(df_ct_mosp_filt$Test_Type=="nmeta2-active")
df_ct_mosp_filt2 <- df_ct_mosp_filt[-factor_to_remove,]
factor_to_remove <- which(df_ct_mosp_filt2$Test_Type=="nmeta2-passive")
df_ct_mosp_filt2 <- df_ct_mosp_filt2[-factor_to_remove,]
df_ct_mosp_filt2$Test_Type <- factor(df_ct_mosp_filt2$Test_Type)
q2 <- ggplot(data=df_ct_mosp_filt2, aes(x=Load_Rate, y=Controller_Pkt_In, fill=Test_Type, color=Test_Type)) + xlab("NFPS Load") + ylab("Controller Packets Received per Interval (pkts)") + theme(legend.title=element_blank()) + scale_x_continuous(limits=c(10, 200)) + geom_point(aes(x=Load_Rate, y=Controller_Pkt_In, color=Test_Type)) + scale_y_continuous(limits=c(0, 200), breaks=c(25,50,75,100,125,150,175,200), labels = comma) + stat_smooth(method = "loess") + theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))
#print (q2)

# Create an nmeta controller cpu chart (Drop nmeta2 factors from DF):
q3 <- ggplot(data=df_ct_mosp_filt2, aes(x=Load_Rate, y=Controller_CPU, fill=Test_Type, color=Test_Type)) + xlab("NFPS Load") + ylab("Controller CPU (%)") + theme(legend.title=element_blank()) + scale_x_continuous(limits=c(10, 200)) + geom_point(aes(x=Load_Rate, y=Controller_CPU, color=Test_Type)) + stat_smooth(method = "loess") + theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))
#print (q3)

#*** Put q1-3 on same page:
#*** Function to hold legend so that it can be shared and placed at will:
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
#*** Add simple titles to differentiate the 3 charts:
q1 <- q1 + ggtitle("HTTP Responsiveness")
q2 <- q2 + ggtitle("Controller Packets")
q3 <- q3 + ggtitle("Controller CPU")
#*** Change legend to top so that it displays horizontally:
q1 <- q1 + theme(legend.position = "top")
#*** Copy legend to a variable:
legend <- get_legend(q1)
#*** Blank plot for cells that don't need a chart in them:
blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()
#*** Remove legends:
q1 <- q1 + theme(legend.position="none")
q2 <- q2 + theme(legend.position="none")
q3 <- q3 + theme(legend.position="none")
#*** Do multiple plots on one page with shared legend:
grid.arrange(legend, blankPlot, blankPlot,
            q1, q2, q3,
             ncol=3, nrow = 2, 
             widths = c(2.3, 2.3, 2.3), heights = c(0.2, 2.5))

#---------------------- COMBINED CPU CHARTS - nmeta --------------------
# Do chart each for nmeta, nmeta2-active and nmeta2-passive showing
# CPU of different components so can see how workload has been moved
# to make it more scalable

#*** Create nmeta data frame with other factors removed:
selected<-c("nmeta")
df_nmeta_ct_cpu <- df_ct_mosp_filt[df_ct_mosp_filt$Test_Type %in% selected,]
df_nmeta_ct_cpu$Test_Type <- factor(df_nmeta_ct_cpu$Test_Type)
#*** Remove superfluous columns:
keeps <- c("Time", "Controller_CPU", "Load_Rate")
df_nmeta_ct_cpu <- df_nmeta_ct_cpu[keeps]
#*** Now a switch CPU data frame:
df_nmeta_sw_cpu <- df_sw_mosp_filt[df_sw_mosp_filt$Test_Type %in% selected,]
df_nmeta_sw_cpu$Test_Type <- factor(df_nmeta_sw_cpu$Test_Type)
#*** Remove superfluous columns:
keeps <- c("Time", "Switch_CPU", "Load_Rate")
df_nmeta_sw_cpu <- df_nmeta_sw_cpu[keeps]

#*** Chart the result:
q_cpu_nmeta <- ggplot() + xlab("\nNFPS Load") + ylab("CPU (%)") + theme(legend.title=element_blank()) + scale_x_continuous(limits=c(10, 500)) + scale_y_continuous(limits=c(0, 100)) + geom_point(data = df_nmeta_ct_cpu, aes(x = Load_Rate, y = Controller_CPU, color = "Controller CPU")) + stat_smooth(method = "loess", data = df_nmeta_ct_cpu, aes(x=Load_Rate, y=Controller_CPU, color = "Controller CPU")) + theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))
#*** Add second series to chart (note different data frame):
q_cpu_nmeta <- q_cpu_nmeta + geom_point(data = df_nmeta_sw_cpu, aes(x=Load_Rate, y=Switch_CPU, color = "Switch CPU")) + stat_smooth(method = "loess", data = df_nmeta_sw_cpu, aes(x=Load_Rate, y=Switch_CPU, color = "Switch CPU")) + scale_color_manual(values=c("#F8766D", "#9ecae1")) 

#---------------------- COMBINED CPU CHARTS - nmeta2-active --------------------
#*** Create nmeta data frame with other factors removed:
selected<-c("nmeta2-active")
df_nmeta2a_ct_cpu <- df_ct_mosp_filt[df_ct_mosp_filt$Test_Type %in% selected,]
df_nmeta2a_ct_cpu$Test_Type <- factor(df_nmeta2a_ct_cpu$Test_Type)
#*** Remove superfluous columns:
keeps <- c("Time", "Controller_CPU", "Load_Rate")
df_nmeta2a_ct_cpu <- df_nmeta2a_ct_cpu[keeps]
#*** Now a switch CPU data frame:
df_nmeta2a_sw_cpu <- df_sw_mosp_filt[df_sw_mosp_filt$Test_Type %in% selected,]
df_nmeta2a_sw_cpu$Test_Type <- factor(df_nmeta2a_sw_cpu$Test_Type)
#*** Remove superfluous columns:
keeps <- c("Time", "Switch_CPU", "Load_Rate")
df_nmeta2a_sw_cpu <- df_nmeta2a_sw_cpu[keeps]
#*** Now a DPAE CPU data frame:
df_nmeta2a_dp_cpu <- df_dp_mosp_filt[df_dp_mosp_filt$Test_Type %in% selected,]
df_nmeta2a_dp_cpu$Test_Type <- factor(df_nmeta2a_dp_cpu$Test_Type)
#*** Remove superfluous columns:
keeps <- c("Time", "DPAE_CPU", "Load_Rate")
df_nmeta2a_dp_cpu <- df_nmeta2a_dp_cpu[keeps]

#*** Chart the CPU result:
q_cpu_nmeta2a <- ggplot() + xlab("\nNFPS Load") + ylab("CPU (%)") + theme(legend.title=element_blank()) + scale_x_continuous(limits=c(10, 500)) + scale_y_continuous(limits=c(0, 100)) + geom_point(data = df_nmeta2a_ct_cpu, aes(x = Load_Rate, y = Controller_CPU, color = "Controller CPU")) + stat_smooth(method = "loess", data = df_nmeta2a_ct_cpu, aes(x=Load_Rate, y=Controller_CPU, color = "Controller CPU")) + theme(axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))
#*** Add second series to chart for switch CPU (note different data frame):
q_cpu_nmeta2a <- q_cpu_nmeta2a + geom_point(data = df_nmeta2a_sw_cpu, aes(x=Load_Rate, y=Switch_CPU, color = "Switch CPU")) + stat_smooth(method = "loess", data = df_nmeta2a_sw_cpu, aes(x=Load_Rate, y=Switch_CPU, color = "Switch CPU"))
#*** Add third series to chart for DPAE CPU (note different data frame):
q_cpu_nmeta2a <- q_cpu_nmeta2a + geom_point(data = df_nmeta2a_dp_cpu, aes(x=Load_Rate, y=DPAE_CPU, color = "DPAE CPU")) + stat_smooth(method = "loess", data = df_nmeta2a_dp_cpu, aes(x=Load_Rate, y=DPAE_CPU, color = "DPAE CPU")) + scale_color_manual(values=c("#F8766D", "#009934", "#9ecae1"))

#-------------- Print COMBINED CPU CHARTS - nmeta, nmeta2-active --------------------
#*** Add simple titles to differentiate the 3 charts:
q_cpu_nmeta <- q_cpu_nmeta + ggtitle("Nmeta Workload")
q_cpu_nmeta2a <- q_cpu_nmeta2a + ggtitle("Nmeta2-Active Workload")
#*** Change legend to top so that it displays horizontally:
q_cpu_nmeta2a <- q_cpu_nmeta2a + theme(legend.position = "top")
#*** Copy legend to a variable:
legend <- get_legend(q_cpu_nmeta2a)
#*** Remove legends:
q_cpu_nmeta <- q_cpu_nmeta + theme(legend.position="none")
q_cpu_nmeta2a <- q_cpu_nmeta2a + theme(legend.position="none")
#*** Do multiple plots on one page with shared legend:
grid.arrange(legend, q_cpu_nmeta, q_cpu_nmeta2a,
            widths = c(2.7, 2.7), heights = c(0.2, 2.5),
            layout_matrix = rbind(c(1,1), c(2,3)))


