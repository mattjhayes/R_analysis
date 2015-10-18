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

# ============================= CSVs to DATA FRAME =====================
fx_csv2df <- function(files_list, files) {
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
# Note: Iperf CSV format is:
# Time, local_IP, local_port, server_IP, server_port, duration, Interval, Transfer, Bandwidth 

# Call function (see further up) to build file data:
files <- fx_build_file_data("pc1.example.com-iperf_result.txt", files_dir_2)
    
print ("Reading Iperf result CSV files into a list")
# Read the result CSV files into a list:
files_list <- lapply(files$files, read.csv, header=FALSE)

# Produce a data frame of just what we need in right format:
df_iperf <- data.frame()
for (i in 1:length(files_list)) {
    test_type <- unname(files$test_types[i])
    dir_path <- unname(files$dir_path[i])
    df_tmp <- data.frame(i)
    colnames(df_tmp)[1] = "Test Number"
    # Add in the 'y' column(s):
    df_tmp[2] <- files_list[[i]][,8]
    df_tmp[3] <- files_list[[i]][,9]
    colnames(df_tmp)[2] <- "Transfer"
    colnames(df_tmp)[3] <- "Bandwidth"
    #*** Add filled Test_Type column:
    df_tmp$Test_Type <- test_type
    #*** Add filled Dir_Path column:
    df_tmp$Dir_Path <- dir_path
    # Accumulate the additional data rows:
    df_iperf = rbind(df_iperf, df_tmp)
}

# ============================= CHARTING ===============================
# Plot results on a chart:
g <- ggplot(df_iperf, aes(x=Test_Type, y=Bandwidth)) +
    geom_point(shape=1) + scale_y_log10() + ggtitle("Static Regression") +
    xlab("Test Type") +
    ylab("Bandwidth (bps - Log10)")
print (g)


