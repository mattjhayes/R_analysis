# Analyse results from a new flow per second (NFPS) load test

# Run in rstudio with:

# source("~/R_analysis/nmeta2-nfps-load-test-analysis.R")

# Imports:
libs <- c('ggplot2', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales', 'zoo')
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
        x <- files_list[[i]]$time
        df_tmp <- data.frame(x)
        colnames(df_tmp)[1] = "Time"
        # Add in the 'y' column(s):
        j <- 2
        for (col_y in col_select) {
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
col_select <- c("ct1.cpu", "ct1.swap.in", "ct1.swap.out", "ct1.pkt.in", "ct1.pkt.out")
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
col_select <- c("sw1.cpu", "sw1.swap.in", "sw1.swap.out")
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
col_select <- c("dp1.cpu", "dp1.swap.in", "dp1.swap.out", "dp1.pkt.in", "dp1.pkt.out")
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
fx_chart_scatter_1("Load_Rate", "Object_Retrieval_Time", "Test_Type", df_cxn_close_filt, "Connection Close Retrieval Time vs New Flows Load by Test Type", "Load Rate", "Object Retrieval Time (seconds)")

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


