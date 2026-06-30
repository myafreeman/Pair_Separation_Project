# Script to take raw SLEAP tracking data and subset it into experimental sessions
  # Sub-setting data to A (test 1) and B (test 2)


#Load packages
library(dplyr)
library(readr)
library(openxlsx)

# Paths
input_folder <- ""     # folder with tracking csv files 
intervals_file <- ""   # file with session onset/offset frame #
output_folder <- ""      # folder to save xlsx

# Make sure output folder exists
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# Read intervals file
# Must have columns: file, onset, offset
intervals <- read_csv(intervals_file)

# List tracking files
files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

# Function to subset and write one file
subset_and_save <- function(file, intervals, output_folder) {
  fname <- basename(file)
  
  # Read tracking data
  dat <- read_csv(file)
  
  # Get onset/offset for this file
  int <- intervals %>% filter(file == fname)
  
  if (nrow(int) == 0) {
    message("No intervals found for: ", fname)
    return(NULL)
  }
  
  # Loop through intervals
  for (i in seq_len(nrow(int))) {
    onset <- int$onset[i]
    offset <- int$offset[i]
    
    subset <- dat %>% filter(frame_idx >= onset & frame_idx <= offset)
    
    # Build output filename (file_interval.xlsx)
    out_name <- paste0(tools::file_path_sans_ext(fname),
                       "_subsetted ", ".xlsx")
    out_path <- file.path(output_folder, out_name)
    
    # Write to xlsx
    write.xlsx(subset, out_path, overwrite = TRUE)
  }
}  

# Apply to all files
lapply(files, subset_and_save, intervals = intervals, output_folder = output_folder)
