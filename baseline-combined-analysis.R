# Analyse results from a new flow per second (NFPS) load test

# Run in rstudio with:

# source("~/R_analysis/baseline-combined-analysis.R")

# Imports:
libs <- c('ggplot2', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales', 'zoo')
lapply(libs, require, character.only = T)

# Base directory where results data is stored:
base_dir <- "~/results/baseline-combined/"

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
    df_combined <-merge(df_results, df_filt, all=T, by="Time")

    # Remove leading rows with NA for filt_Actual_Rate as they precede the
    #  start of the test:
    first_row <- which.min(is.na(df_combined$Previous_Actual_Rate))
    print(paste0("fx_index_by_load: First row with filt test running is ", first_row))
    df_combined = df_combined[-(1:(first_row-1)),]

    print("fx_index_by_load: Zoo time! Replace the NAs with next value in column")
    # Use zoo package na.locf
    df_combined$Previous_Actual_Rate <- na.locf(df_combined$Previous_Actual_Rate, fromLast = TRUE, na.rm = FALSE)

    print("fx_index_by_load: remove NAs")
    # subset df with only rows that have complete data in column 2:
    df_combined <- df_combined[complete.cases(df_combined[, 2]),]

    print("fx_index_by_load: remove superfluous columns and tidy up names")
    drops <- c("Test_Type.y","Dir_Path.y")
    df_combined <- df_combined[,!(names(df_combined) %in% drops)]
    colnames(df_combined)[names(df_combined)=="Test_Type.x"] <- "Test_Type"
    colnames(df_combined)[names(df_combined)=="Dir_Path.x"] <- "Dir_Path"
    colnames(df_combined)[names(df_combined)=="Previous_Actual_Rate"] <- "Load_Rate"
    return(df_combined)
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
files_ct_mosp <- vector()
test_types_ct_mosp <- vector()
dir_path_ct_mosp <- vector()

for (test_type in files_dir_2) {
  base_dir_3 <- paste(base_dir_2, test_type, sep = '/')
  files_dir_3 <- list.files(path=base_dir_3)
  for (test_timestamp in files_dir_3) {
    base_dir_4 <- paste(base_dir_3, test_timestamp, sep = '/')
    files_dir_4 <- list.files(path=base_dir_4)
    if (is.element("ct1.example.com-mosp.csv", files_dir_4)) {
      full_path = paste(base_dir_4, "ct1.example.com-mosp.csv", sep = '/')
      # Append the full path of the file to the list
      files_ct_mosp <- c(files_ct_mosp, full_path)
      # Use test_types to hold mapping between full file path and type of test:
      test_types_ct_mosp[full_path] <- test_type
      # Store the directory path:
      dir_path_ct_mosp[full_path] <- base_dir_4
    }
  }
}

print ("Reading controller mosp result CSV files into a list")
# Read the controller mosp result CSV files into a list:
files_list_ct_mosp <- lapply(files_ct_mosp, read.csv)

# Data frame for controller mosp results:
# Set a blank data frame to put our results into:
df_ct_mosp = data.frame()
for (i in 1:length(files_list_ct_mosp)) {
  test_type <- unname(test_types_ct_mosp[i])
  dir_path <- unname(dir_path_ct_mosp[i])
  x <- files_list_ct_mosp[[i]]$time
  y1 <- files_list_ct_mosp[[i]]$ct1.cpu
  y2 <- files_list_ct_mosp[[i]]$ct1.swap.in
  y3 <- files_list_ct_mosp[[i]]$ct1.swap.out
  y4 <- files_list_ct_mosp[[i]]$ct1.pkt.in
  y5 <- files_list_ct_mosp[[i]]$ct1.pkt.out
  #*** fill vector z1 with the test type:
  z1 <- rep(test_type, length(x))
  #*** fill vector z2 with the directory path:
  z2 <- rep(dir_path, length(x))
  d = data.frame(x, y1, y2, y3, y4, y5, z1, z2)
  # Set appropriate column names.
  colnames(d) <- c("Time", "Controller_CPU", "Controller_Swap_In", "Controller_Swap_Out", "Controller_Pkt_In", "Controller_Pkt_Out", "Test_Type", "Dir_Path")
  # Accumulate the additional data rows:
  df_ct_mosp = rbind(df_ct_mosp, d)
}
# Set Time column to POSIXct data type:
df_ct_mosp$Time <- as.POSIXct(df_ct_mosp$Time)

# Create a data frame that is indexed by filt NFPS load:
df_ct_mosp_filt = fx_index_by_load(df_ct_mosp, df_filt)

# ===================== Switch OS analysis:
files_sw_mosp <- vector()
test_types_sw_mosp <- vector()
dir_path_sw_mosp <- vector()

for (test_type in files_dir_2) {
  base_dir_3 <- paste(base_dir_2, test_type, sep = '/')
  files_dir_3 <- list.files(path=base_dir_3)
  for (test_timestamp in files_dir_3) {
    base_dir_4 <- paste(base_dir_3, test_timestamp, sep = '/')
    files_dir_4 <- list.files(path=base_dir_4)
    if (is.element("sw1.example.com-mosp.csv", files_dir_4)) {
      full_path = paste(base_dir_4, "sw1.example.com-mosp.csv", sep = '/')
      # Append the full path of the file to the list
      files_sw_mosp <- c(files_sw_mosp, full_path)
      # Use test_types to hold mapping between full file path and type of test:
      test_types_sw_mosp[full_path] <- test_type
      # Store the directory path:
      dir_path_sw_mosp[full_path] <- base_dir_4
    }
  }
}

print ("Reading Switch mosp result CSV files into a list")
# Read the Switch mosp result CSV files into a list:
files_list_sw_mosp <- lapply(files_sw_mosp, read.csv)

# Data frame for Switch mosp results:
# Set a blank data frame to put our results into:
df_sw_mosp = data.frame()
for (i in 1:length(files_list_sw_mosp)) {
  test_type <- unname(test_types_sw_mosp[i])
  dir_path <- unname(dir_path_sw_mosp[i])
  x <- files_list_sw_mosp[[i]]$time
  y1 <- files_list_sw_mosp[[i]]$sw1.cpu
  y2 <- files_list_sw_mosp[[i]]$sw1.swap.in
  y3 <- files_list_sw_mosp[[i]]$sw1.swap.out
  y4 <- files_list_sw_mosp[[i]]$sw1.pkt.in
  y5 <- files_list_sw_mosp[[i]]$sw1.pkt.out
  #*** fill vector z1 with the test type:
  z1 <- rep(test_type, length(x))
  #*** fill vector z2 with the directory path:
  z2 <- rep(dir_path, length(x))
  d = data.frame(x, y1, y2, y3, y4, y5, z1, z2)
  # Set appropriate column names.
  colnames(d) <- c("Time", "Switch_CPU", "Switch_Swap_In", "Switch_Swap_Out", "Switch_Pkt_In", "Switch_Pkt_Out", "Test_Type", "Dir_Path")
  # Accumulate the additional data rows:
  df_sw_mosp = rbind(df_sw_mosp, d)
}
# Set Time column to POSIXct data type:
df_sw_mosp$Time <- as.POSIXct(df_sw_mosp$Time)

# Create a data frame that is indexed by filt NFPS load:
df_sw_mosp_filt = fx_index_by_load(df_sw_mosp, df_filt)

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
# Set Time column to POSIXct data type:
df_cxn_close$Time <- as.POSIXct(df_cxn_close$Time)

# Create a data frame that is indexed by filt NFPS load:
df_cxn_close_filt = fx_index_by_load(df_cxn_close, df_filt)

# ===================== cxn-keepalive analysis:
files_cxn_keepalive <- vector()
test_types_cxn_keepalive <- vector()
dir_path_cxn_keepalive <- vector()

for (test_type in files_dir_2) {
  base_dir_3 <- paste(base_dir_2, test_type, sep = '/')
  files_dir_3 <- list.files(path=base_dir_3)
  for (test_timestamp in files_dir_3) {
    base_dir_4 <- paste(base_dir_3, test_timestamp, sep = '/')
    files_dir_4 <- list.files(path=base_dir_4)
    if (is.element("pc1.example.com-hort-cxn-keepalive.csv", files_dir_4)) {
      full_path = paste(base_dir_4, "pc1.example.com-hort-cxn-keepalive.csv", sep = '/')
      # Append the full path of the file to the list
      files_cxn_keepalive <- c(files_cxn_keepalive, full_path)
      # Use test_types to hold mapping between full file path and type of test:
      test_types_cxn_keepalive[full_path] <- test_type
      # Store the directory path:
      dir_path_cxn_keepalive[full_path] <- base_dir_4
    }
  }
}

print ("Reading hort client cxn-keepalive result CSV files into a list")
# Read the pc1 connection keepalive csv files into a list:
files_list_cxn_keepalive <- lapply(files_cxn_keepalive, read.csv)

# Data frame for cxn-keepalive object retrieval times:
# Pull out the values we need and merge into a single data frame
#  with a column of retrieval times, a column for test type,
#  indexed against target rate:
# Set a blank data frame to put our results into:
df_cxn_keepalive = data.frame()
for (i in 1:length(files_list_cxn_keepalive)) {
  test_type <- unname(test_types_cxn_keepalive[i])
  dir_path <- unname(dir_path_cxn_keepalive[i])
  x <- files_list_cxn_keepalive[[i]]$time
  y <- files_list_cxn_keepalive[[i]]$pc1.cxn.keep.alive.retrieval.time
  #*** fill vector z1 with the test type:
  z1 <- rep(test_type, length(x))
  #*** fill vector z2 with the directory path:
  z2 <- rep(dir_path, length(x))
  d = data.frame(x, y, z1, z2)
  # Set appropriate column names.
  colnames(d) <- c("Time", "Object_Retrieval_Time", "Test_Type", "Dir_Path")
  # Accumulate the additional data rows:
  df_cxn_keepalive = rbind(df_cxn_keepalive, d)
}
# Set Time column to POSIXct data type:
df_cxn_keepalive$Time <- as.POSIXct(df_cxn_keepalive$Time)

# Create a data frame that is indexed by filt NFPS load:
df_cxn_keepalive_filt = fx_index_by_load(df_cxn_keepalive, df_filt)

#============================== NMETA EVENT RATES ===============================
# Note: KVP format, so needs some special handling

files_nmev <- vector()
test_types_nmev <- vector()
dir_path_nmev <- vector()
df_nmev = data.frame()

for (test_type in files_dir_2) {
  base_dir_3 <- paste(base_dir_2, test_type, sep = '/')
  files_dir_3 <- list.files(path=base_dir_3)
  for (test_timestamp in files_dir_3) {
    base_dir_4 <- paste(base_dir_3, test_timestamp, sep = '/')
    files_dir_4 <- list.files(path=base_dir_4)
    if (is.element("ct1.example.com-hort-nmeta-eventrates.csv", files_dir_4)) {
      full_path = paste(base_dir_4, "ct1.example.com-hort-nmeta-eventrates.csv", sep = '/')
      # Append the full path of the file to the list
      files_nmev <- c(files_nmev, full_path)
      # Use test_types to hold mapping between full file path and type of test:
      test_types_nmev[full_path] <- test_type
      # Store the directory path:
      dir_path_nmev[full_path] <- base_dir_4
    }
  }
}

print ("Reading nmeta event rate CSV files into a list")
# Read the pc1 connection close csv files into a list:
files_list_nmev <- lapply(files_nmev, read.csv)

# nmeta event rate KVP processing (not very good R...):
print ("Processing nmeta event rate KVP to data frame")
for (h in 1:length(files_list_nmev)) {
    file1 <- files_list_nmev[[h]]
    test_type <- unname(test_types_nmev[[h]])
    dir_path <- unname(dir_path_nmev[[h]])
    for(i in 1:nrow(file1)) {
        # To get a df row as a simple vector have to do some trickery
        # Need to transform (x<->y) then grab column as vector...
        # Extract a row as unnamed vector from the data frame
        row <- unname(c(t(file1)[,i]))
        # Turn time field into a nmev:
        row[1] <- paste0("Time=", row[1])
        # Conditionally remove last element (occurs if CSV with trailing comma)
        if (is.na(tail(row, n=1))) {
            row <- row[-length(row)]
        }
        #row <- c(row, full_path)
        # Turn vector into a data frame:
        df_row <- as.data.frame(sapply(strsplit(row, '='), rbind), stringsAsFactors=FALSE)
        # Fix names:
        names(df_row) <- df_row[1,]
        df_row <- df_row[-1,]
        # Add the test type and dir path values:
        df_row$Test_Type <- test_type
        df_row$Dir_Path <- dir_path
        # Merge into result df:
        if("Time" %in% colnames(df_nmev)) {
            df_nmev <-merge(df_nmev, df_row, all=TRUE, sort=TRUE)
        }
        else {
            df_nmev <- df_row
        }
    }
}
# Set Time column to POSIXct data type:
df_nmev$Time <- as.POSIXct(df_nmev$Time)

# Create a data frame that is indexed by filt NFPS load:
df_nmev_filt = fx_index_by_load(df_nmev, df_filt)

# ============================= CHARTING ===============================

# Call our function to create charts (see top of this program)

# Packet-in chart:
print("Packet-in: creating chart")
fx_chart_scatter_1("Load_Rate", "packet_in", "Test_Type", df_nmev_filt, "Controller OpenFlow packet-in rate vs New Flows Load", "Load Rate", "Add Flow Rate")

# Add flow chart:
print("Add-flow: creating chart")
fx_chart_scatter_1("Load_Rate", "add_flow", "Test_Type", df_nmev_filt, "Controller OpenFlow Add Flow Rate", "Load Rate", "Add Flow Rate")

# Cxn-close chart:
print("Client cxn-close: creating chart")
fx_chart_scatter_1("Load_Rate", "Object_Retrieval_Time", "Test_Type", df_cxn_close_filt, "Connection Close Retrieval Time vs New Flows Load by Test Type", "Load Rate", "Object Retrieval Time (seconds)")

# Cxn-keepalive chart:
print("Client cxn-keepalive: creating chart")
fx_chart_scatter_1("Load_Rate", "Object_Retrieval_Time", "Test_Type", df_cxn_keepalive_filt, "Connection Keepalive Retrieval Time vs New Flows Load by Test Type", "Load Rate", "Object Retrieval Time (seconds)")

# Controller CPU:
print("Controller mosp: creating CPU chart")
fx_chart_scatter_1("Load_Rate", "Controller_CPU", "Test_Type", df_ct_mosp_filt, "Controller CPU vs New Flows Load by Test Type", "Load Rate", "CPU Load (%)")

# Controller Swap Out:
print("Controller mosp: creating Swap Out chart")
fx_chart_scatter_1("Load_Rate", "Controller_Swap_Out", "Test_Type", df_ct_mosp_filt, "Controller Swap Out vs New Flows Load by Test Type", "Load Rate", "Swap Out (Bytes) per interval")

# Controller Ethernet Packets In:
print("Controller mosp: creating Packets In chart")
fx_chart_scatter_1("Load_Rate", "Controller_Pkt_In", "Test_Type", df_ct_mosp_filt, "Controller Ethernet Packets In vs New Flows Load by Test Type", "Load Rate", "Packets Received per Interval")

# Switch CPU:
print("Switch mosp: creating CPU chart")
fx_chart_scatter_1("Load_Rate", "Switch_CPU", "Test_Type", df_sw_mosp_filt, "Switch CPU vs New Flows Load by Test Type", "Load Rate", "Switch CPU (%)")

# Switch Swap Out:
print("Switch mosp: creating Swap Out chart")
fx_chart_scatter_1("Load_Rate", "Switch_Swap_Out", "Test_Type", df_sw_mosp_filt, "Switch Swap Out vs New Flows Load by Test Type", "Load Rate", "Swap Out (Bytes) per interval")



