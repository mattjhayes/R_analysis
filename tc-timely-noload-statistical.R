# Analyse results from a timeliness no-load statistical test

# Run in rstudio with:

# source("~/R_analysis/tc-timely-noload-statistical.R")

# Imports:
libs <- c('ggplot2', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales', 'zoo')
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

#======================== Treatment Time ===============================
# Call function (see further up) to build file data:
files_tt <- fx_build_file_data("post_process_treatment_time_delta.txt", files_dir_2)

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
files_p2dpae <- fx_build_file_data("post_process_dpae_pkts.txt", files_dir_2)

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
#df_combined = rbind(df_tt, df_p2dpae)
df_combined <- merge(df_tt, df_p2dpae,by="Test Number")
# Fix double-up of columns:
drops <- c("Test_Type.y","Dir_Path.y")
df_combined <- df_combined[,!(names(df_combined) %in% drops)]
colnames(df_combined)[names(df_combined)=="Test_Type.x"] <- "Test_Type"
colnames(df_combined)[names(df_combined)=="Dir_Path.x"] <- "Dir_Path"

# ============================= CHARTING ===============================
# Plot results on a chart:
g <- ggplot(df_iperf_pc, aes(x=Hostname, y=Bandwidth)) +
    geom_point(shape=1) +
    facet_grid(. ~ Test_Type, labeller=fx_chart_facet_labeller) +
    scale_y_log10() + ggtitle("Statistical Regression Tests") +
    xlab("Hostname") +
    ylab("Bandwidth (bps - Log10)")
print (g)

# Packets to DPAE vs Time to Treat:
print("Creating chart for Packets to DPAE vs Time to Treat")
fx_chart_scatter_1("Time_to_Treatment", "Packets_to_DPAE", "Test_Type", df_combined, "Packets to DPAE vs Time to Treat", "Time to Treatment", "Packets to DPAE")

# Packets to DPAE vs Time to Treat 2:
print("Creating chart for Packets to DPAE vs Time to Treat 2")
q <- qplot(df_combined$"Time_to_Treatment", df_combined$"Packets_to_DPAE", color=df_combined$"Test_Type", main="Packets to DPAE vs Time to Treat", xlab="Time to Treatment (seconds)", ylab="Packets to DPAE (packets)")
q + labs(color="custom title")
print (q)
