# Clean and process sleap tracking data -- Implementing DLC_analyzer code --
# Sturman et al, 2020 (DOI:10.1038/s41386-020-0776-y) on SLEAP data.

#load in required packages 
options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/latest"))
library(readxl)
library(writexl)
library(sp)         #tested with v1.3-2
library(imputeTS)   #tested with v3.0
library(ggplot2)    #tested with v3.1.0
library(ggmap)      #tested with v3.0.0
library(data.table) #tested with v1.12.8
library(cowplot)    #tested with v0.9.4
library(corrplot)   #tested with v0.84

#source functions from DLCAnalyzer_Functions_final.r (DOI:10.1038/s41386-020-0776-y)
  # Make sure the DLCAnalyzer_Functions_final.R is downloaded and change path_name to current path 
source('path_name/DLCAnalyzer_Functions_final.R')

# Define input folder containing csv files of tracking data
  #ensure birdID included in file name 
input_folder <- "" #Change for parsed, DLC formation files location 

#define output folder for processed files 
output_folder <- "" 

# List all CSV files in the input folder
files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)




# Functions # 

#Interpolate missing frames between first and last frame for each body part
  # For subsetted dfs with variable start and end frame numbers  
  # ensures every frame number over experiment has an estimated location 
  # Interpolated values will not have likelihood scores - distinction between model predicted and interpolated data 
  # save raw data for comparison of partial skeleton detections 
InterpolateTrackingData_perSubset <- function(tracking,
                                              method = "linear",
                                              mark_interpolated = FALSE) {
  
  if (!is.list(tracking) || is.null(tracking$data)) {
    stop("tracking must be a TrackingData-like object with a 'data' list.")
  }
  
  if (!method %in% c("linear", "spline", "stine")) {
    stop("method must be one of 'linear', 'spline', or 'stine'.")
  }
  
  out <- tracking
  
  out$data <- lapply(tracking$data, function(df) {
    
    if (!"frame" %in% names(df)) {
      stop("Each bodypart data.frame must contain a 'frame' column.")
    }
    
    df$frame <- as.integer(df$frame)
    
    if (all(is.na(df$frame))) return(df)
    
    minf <- min(df$frame, na.rm = TRUE)
    maxf <- max(df$frame, na.rm = TRUE)
    
    full <- data.frame(frame = seq(minf, maxf))
    
    df_full <- full %>%
      dplyr::left_join(df, by = "frame") %>%
      dplyr::arrange(frame)
    
    ## ---- store raw coordinates ----
    df_full$x_raw <- df_full$x
    df_full$y_raw <- df_full$y
    
    if (mark_interpolated) {
      orig_missing <- is.na(df_full$x_raw) | is.na(df_full$y_raw)
    }
    
    ## ---- interpolate into x / y ----
    df_full$x <- imputeTS::na_interpolation(df_full$x, option = method)
    df_full$y <- imputeTS::na_interpolation(df_full$y, option = method)
    
    if (mark_interpolated) {
      filled_now <- !is.na(df_full$x) & !is.na(df_full$y)
      df_full$interpolated <- orig_missing & filled_now
    }
    
    df_full
  })
  
  frames_union <- sort(unique(unlist(lapply(out$data, function(df) df$frame))))
  out$frames <- frames_union
  out$seconds <- out$frames / out$fps
  
  out
}


# Function to convert a tracking object from pixels to cm based on resolution 
  # video_height = (720px, 1080px,...)
  # arena_height_in = (width size of cage)
  # adds cm column while retaining original pxl coordinates 
convert_tracking_to_cm <- function(tracking, video_height, arena_height_in = 8) {
  # Convert inches → cm
  arena_height_cm <- arena_height_in * 2.54
  
  # Scaling factor (cm per pixel)
  pixel_to_cm <- arena_height_cm / video_height
  message("Scaling with factor: ", round(pixel_to_cm, 5), " cm/px (video height = ", video_height, ")")
  
  # Apply to each bodypart's x/y, but keep likelihood untouched
  for (bp in names(tracking$data)) {
    tracking$data[[bp]]$x_cm <- tracking$data[[bp]]$x * pixel_to_cm
    tracking$data[[bp]]$y_cm <- tracking$data[[bp]]$y * pixel_to_cm
    tracking$data[[bp]]$x_raw_cm <- tracking$data[[bp]]$x_raw * pixel_to_cm
    tracking$data[[bp]]$y_raw_cm <- tracking$data[[bp]]$y_raw * pixel_to_cm
  }
  
  # Update median.data as well
  tracking$median.data$x_cm <- tracking$median.data$x * pixel_to_cm
  tracking$median.data$y_cm <- tracking$median.data$y * pixel_to_cm
  
  # Record new distance units
  tracking$distance.units <- "cm"
  attr(tracking, "pixel_to_cm") <- pixel_to_cm
  
  return(tracking)
}



# CLEAN AND PROCESS LOOP -----------------------------------------------------
  # Loop to process a batch of files at once 
  # save raw data for comparison of partial skeleton detection 
  # save raw and interpolated data 
  # derived values (speed acceleration...) from raw values not interpolated 
# Loop through each file 
for (file in files) {
  
  # Extract file name without extension
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Read and clean tracking data
  Tracking <- ReadDLCDataFromCSV(file = file, fps = 30)
  #Tracking <- CleanTrackingData(Tracking, likelihoodcutoff = 0.50)
  Tracking<- InterpolateTrackingData_perSubset(Tracking, method = "linear", mark_interpolated = TRUE)
  Tracking <- convert_tracking_to_cm(Tracking, video_height = 1080, arena_height_in = 8) 
  
  # Initialize vectors
  frames <- c()
  x_head <- c(); y_head <- c()
  x_head_raw <- c(); y_head_raw <- c()
  x_beak <- c(); y_beak <- c()
  x_beak_raw <- c(); y_beak_raw <- c()
  likelihood_head <- c()
  likelihood_beak <- c()
  
  
  # Loop through frames and extract both Head and Beak positions (cm units)
  for (i in seq_along(Tracking$frames)) {
    frames <- c(frames, Tracking[["frames"]][[i]])
    x_head <- c(x_head, Tracking[["data"]][["Head"]][["x_cm"]][[i]])
    y_head <- c(y_head, Tracking[["data"]][["Head"]][["y_cm"]][[i]])
    likelihood_head <- c(likelihood_head, Tracking[["data"]][["Head"]][["likelihood"]][[i]])
    x_head_raw <- c(x_head_raw, Tracking[["data"]][["Head"]][["x_raw_cm"]][[i]])
    y_head_raw <- c(y_head_raw, Tracking[["data"]][["Head"]][["y_raw_cm"]][[i]])
    x_beak <- c(x_beak, Tracking[["data"]][["Beak"]][["x_cm"]][[i]])
    y_beak <- c(y_beak, Tracking[["data"]][["Beak"]][["y_cm"]][[i]])
    likelihood_beak <- c(likelihood_beak, Tracking[["data"]][["Beak"]][["likelihood"]][[i]])
    x_beak_raw <- c(x_beak_raw, Tracking[["data"]][["Beak"]][["x_raw_cm"]][[i]])
    y_beak_raw <- c(y_beak_raw, Tracking[["data"]][["Beak"]][["y_raw_cm"]][[i]])
  }
  
  # Create a combined data frame
  df <- data.frame(
    Frame = frames,
    x_head = x_head,
    y_head = y_head,
    x_head_raw = x_head_raw,
    y_head_raw = y_head_raw,
    likelihood_head = likelihood_head,
    x_beak = x_beak,
    y_beak = y_beak,
    x_beak_raw = x_beak_raw,
    y_beak_raw = y_beak_raw,
    likelihood_beak = likelihood_beak
  )
  
  # Compute deltas, speed, acceleration for head (cm per frame)
  df$delta_x_head <- c(0, diff(df$x_head_raw))
  df$delta_y_head <- c(0, diff(df$y_head_raw))
  df$speed_head_pframe <- sqrt(df$delta_x_head^2 + df$delta_y_head^2)  # cm/frame
  
  # Convert to cm/s using fps
  df$speed_head_psec <- df$speed_head_pframe * Tracking$fps
  df$acceleration_head_psec <- c(0, diff(df$speed_head_psec)) * Tracking$fps  # cm/s²
  
  # Compute deltas, speed, acceleration for beak
  df$delta_x_beak <- c(0, diff(df$x_beak_raw))
  df$delta_y_beak <- c(0, diff(df$y_beak_raw))
  df$speed_beak_pframe <- sqrt(df$delta_x_beak^2 + df$delta_y_beak^2)  # cm/frame
  
  # Convert to cm/s
  df$speed_beak_psec <- df$speed_beak_pframe * Tracking$fps
  df$acceleration_beak_psec <- c(0, diff(df$speed_beak_psec)) * Tracking$fps  # cm/s²
  
  # Add sequential frame index
  df$adj_frame <- seq_len(nrow(df))
  
  # Define output file path
  output_file <- file.path(output_folder, paste0(file_name, "_processed.xlsx"))
  
  # Write to Excel
  write_xlsx(df, output_file)
  
  # Print progress
  cat("Processed and saved:", output_file, "\n") #ensure all files were processed and saved
}



