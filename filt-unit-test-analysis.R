# Analyse results from a filt unit test

# Run in rstudio with:

# source("/home/bob/analysis/filt-unit-test-analysis.R")

# Imports:
libs <- c('ggplot2', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales')
lapply(libs, require, character.only = T)

# Base directory where results data is stored:
base_dir <- "/home/bob/results/filt-unit-tests/"

# Ask for the sub-directory that the results are in:
#test_dir_1 <- readline("What is name of directory?")
test_dir_1 <- "20150913191540"

base_dir_2 <- paste(base_dir, test_dir_1, sep = '')

print(paste0("Looking for test result data in ", base_dir_2))

files_dir_2 <- list.files(path=base_dir_2)

print(paste0("Found ", files_dir_2))

# Sanity check:
if (length(files_dir_2) < 1) {
  stop("No subdirectories?")
}

file_names <- vector()
test_types <- vector()

# Read in the lg1.example.com-filt-*.csv files from the various
#  sub-directories:
for (test_type in files_dir_2) {
  base_dir_3 <- paste(base_dir_2, test_type, sep = '/')
  files_dir_3 <- list.files(path=base_dir_3)
  for (test_timestamp in files_dir_3) {
    base_dir_4 <- paste(base_dir_3, test_timestamp, sep = '/')
    files_dir_4 <- list.files(path=base_dir_4)
    for (file_name in files_dir_4) {
      file_match <- grep("lg1\\.example\\.com\\-filt\\-.*\\.csv", file_name)
      if (length(file_match) > 0) {
        if (file_match == 1) {
          full_path = paste(base_dir_4, file_name, sep = '/')
          # Append the full path of the file to the list
          file_names  <- c(file_names, full_path)
          # Use test_types to hold mapping between full file path and type of test:
          test_types[full_path] <- test_type
        }
      }
    }
  }
}
print (paste0("file_names is ", file_names))
#print (paste0("file_types is ", file_types))

# Read the csv files into a list:
files_list <- lapply(file_names, read.csv)

# Data frame for actual rates:
# Pull out the values we need and merge into a single data frame
#  with a column of actual rates, a column for test type,
#  indexed against target rate:
# Set a blank data frame to put our results into:
df_actual_rate = data.frame()
for (i in 1:length(files_list)) {
  test_type <- unname(test_types[i])
  x <- files_list[[i]]$previous.target.rate.pps.
  y <- files_list[[i]]$previous.actual.rate.pps.
  z <- rep(test_type, length(x))
  d = data.frame(x, y, z)
  # Set appropriate column names.
  colnames(d) <- c("Target_Rate", "Actual_Rate", "Test_Type")
  # Accumulate the additional data rows:
  df_actual_rate = rbind(df_actual_rate, d)
}

# Data frame for interpacket min and max rates:
# Pull out the values we need and merge into a single data frame
#  with columns for min and max rates, differences against the target
#  rate and a column for test type,
#  indexed against target rate:
# Set a blank data frame to put our results into:
df_minmax_rate = data.frame()
for (i in 1:length(files_list)) {
  test_type <- unname(test_types[i])
  x <- files_list[[i]]$previous.target.rate.pps.
  y1 <- files_list[[i]]$previous.interpacket.min.rate
  y1b <- files_list[[i]]$previous.target.rate.pps. - files_list[[i]]$previous.interpacket.min.rate
  y2 <- files_list[[i]]$previous.interpacket.max.rate
  y2b <- files_list[[i]]$previous.interpacket.max.rate - files_list[[i]]$previous.target.rate.pps.
  t <- rep(test_type, length(x))
  d = data.frame(x, y1, y1b, y2, y2b, t)
  # Set appropriate column names.
  colnames(d) <- c("Target_Rate", "Min_Rate", "Rel_Min_Rate", "Max_Rate", "Rel_Max_Rate", "Test_Type")
  # Accumulate the additional data rows:
  df_minmax_rate = rbind(df_minmax_rate, d)
}

# Actual Rate:
# Scatter lattice with panel per test type and R squared stat analysis:
scatter.lattice.ar <- xyplot(Actual_Rate ~ Target_Rate | Test_Type, 
                          data = df_actual_rate,
                          main="Actual Rate vs Target Rate by Test Type",
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
                          as.table = TRUE)
p = scatter.lattice.ar
print (p)

# Min Rate:
# Scatter lattice with panel per test type and R squared stat analysis:
scatter.lattice.min <- xyplot(Min_Rate ~ Target_Rate | Test_Type, 
                          data = df_minmax_rate,
                          main="Minimum Rate vs Target Rate by Test Type",
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
                          as.table = TRUE)
p = scatter.lattice.min
print (p)

# Rel Min Rate:
# Scatter lattice with panel per test type and R squared stat analysis:
scatter.lattice.min <- xyplot(Rel_Min_Rate ~ Target_Rate | Test_Type, 
                          data = df_minmax_rate,
                          main="Relative Minimum Rate vs Target Rate by Test Type",
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
                          as.table = TRUE)
p = scatter.lattice.min
print (p)

# Max Rate:
# Scatter lattice with panel per test type and R squared stat analysis:
scatter.lattice.max <- xyplot(Max_Rate ~ Target_Rate | Test_Type, 
                          data = df_minmax_rate,
                          main="Max Rate vs Target Rate by Test Type",
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
                          as.table = TRUE)
p = scatter.lattice.max
print (p)

# Rel Max Rate:
# Scatter lattice with panel per test type and R squared stat analysis:
scatter.lattice.max <- xyplot(Rel_Max_Rate ~ Target_Rate | Test_Type, 
                          data = df_minmax_rate,
                          main="Rel Max Rate vs Target Rate by Test Type",
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
                          as.table = TRUE)
p = scatter.lattice.max
print (p)

# ====================================================================
# Examples of lattice using colorspace to show point density:
my.theme <- trellis.par.get()
my.theme$strip.background$col <- "grey80"
my.theme$plot.symbol$pch <- 16
my.theme$plot.symbol$col <- "grey60"
my.theme$plot.polygon$col <- "grey90"

clrs.spec <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
clrs.hcl <- function(n) {
  hcl(h = seq(230, 0, length.out = n), 
      c = 60, l = seq(10, 90, length.out = n), 
      fixup = TRUE)
  }

### function to plot a colour palette
pal <- function(col, border = "transparent", ...)
{
 n <- length(col)
 plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
      axes = FALSE, xlab = "", ylab = "", ...)
 rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

# Lattice Smooth Scatter:

xy <- kde2d(x = df_actual_rate$Target_Rate, y = df_actual_rate$Actual_Rate, n = 100) 
xy.tr <- con2tr(xy)
offset <- max(xy.tr$z) * 0.2
z.range <- seq(min(xy.tr$z), max(xy.tr$z) + offset, offset * 0.01)

l.sc <- update(scatter.lattice.ar, aspect = 1, par.settings = my.theme, 
               between = list(x = 0.3, y = 0.3),
               panel=function(x,y) {
                 xy <- kde2d(x,y, n = 100) 
                 xy.tr <- con2tr(xy)                 
                 panel.levelplot(xy.tr$x, xy.tr$y, xy.tr$z, asp = 1,
                                 subscripts = seq(nrow(xy.tr)), 
                                 contour = FALSE, region = TRUE, 
                                 col.regions = c("white", 
                                                 rev(clrs.hcl(10000))),
                                 at = z.range)
                 lm1 <- lm(y ~ x)
                 lm1sum <- summary(lm1)
                 r2 <- lm1sum$adj.r.squared
                 panel.abline(a = lm1$coefficients[1], 
                              b = lm1$coefficients[2])
                 panel.text(labels = 
                              bquote(italic(R)^2 == 
                                       .(format(r2, digits = 3))),
                            x = 4, y = 1000)
                 #panel.xyplot(x,y) 
                 } 
               ) 

l.sc.smooth <- update(scatter.lattice.ar, aspect = 1, 
                      par.settings = my.theme, 
                      between = list(x = 0.3, y = 0.3),
                      panel = panel.smoothScatter)

print(l.sc.smooth)


# =================== Grid test to put colour legend on lattice scatter:
grid.newpage()
#grid.rect()
print(l.sc, newpage = FALSE)
#grid.rect()
downViewport(trellis.vpname(name = "figure"))
#grid.rect()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.5, width = 0.3,
                just = c("right", "bottom"),
                name = "legend.vp")

pushViewport(vp1)
#grid.rect()

vp1.1 <- viewport(x = 0.2, y = 0.5, 
                  height = 0.7, width = 0.5,
                  just = c("left", "centre"),
                  name = "legend.key")

pushViewport(vp1.1)
#grid.rect()

xy <- kde2d(x = df_actual_rate$Target_Rate, y = df_actual_rate$Actual_Rate, n = 100) 
xy.tr <- con2tr(xy)

offset <- max(xy.tr$z) * 0.01
z.range <- seq(min(xy.tr$z), max(xy.tr$z) + 50 * offset, offset)

key <- draw.colorkey(key = list(col = c("white", rev(clrs.hcl(10000))),
                                at = z.range), draw = TRUE)

seekViewport("legend.vp")
#grid.rect()
vp1.2 <- viewport(x = 1, y = 0.5, 
                  height = 1, width = 0.3,
                  just = c("right", "centre"),
                  name = "legend.text", angle = 0)

pushViewport(vp1.2)
#grid.rect()

grid.text("estimated point density", 
          x = 0, y = 0.5, rot = 270, 
          just = c("centre", "bottom"))

upViewport(3)


