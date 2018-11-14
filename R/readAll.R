

# # create vector of file paths for run1 and run2
# path1 <- list.files("Data/V16D_run_week_20180509/final_spreadsheets", full.names = TRUE)
# path2 <- list.files("Data/V16D_run_week_20180516/final_spreadsheets", full.names = TRUE)
# 
# # combine the 2 vector paths
# fullpath <- c(path1, path2)
# 
# # read in the data and combine together into 1 data.table
# data_raw <- rbindlist(lapply(fullpath, function(x) fread(
#   x, sep = ",", header = TRUE)), 
#   fill = TRUE, use.names = TRUE)