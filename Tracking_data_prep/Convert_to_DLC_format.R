# To use cleaning scripts from (Sturman et al, 2020 (DOI:10.1038/s41386-020-0776-y) need to convert
# column names to DLC Format


# Paths
input_folder  <- ""     # folder with subsetted CSV/XLSX files
output_folder <- ""     # folder to save DLC-style files

if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# Function to convert to DLC-style
convert_to_dlc_format <- function(df, scorer = "scorer") {
  # Drop unwanted columns
  df <- df %>% select(-track, -instance.score)
  
  # Save frame_idx separately (this becomes first column)
  frame_idx <- df$frame_idx
  df <- df %>% select(-frame_idx)
  
  # Split names like "Beak.x" → "Beak", "x"
  parts <- strsplit(names(df), "\\.")
  
  # Build header rows
  row1 <- c("scorer", rep(scorer, length(parts)))
  row2 <- c("bodyparts", sapply(parts, function(x) x[1]))
  row3 <- c("coords", sapply(parts, function(x) {
    if (length(x) > 1) {
      if (x[2] == "score") "likelihood" else x[2]
    } else {
      x[1]
    }
  }))
  
  # Bind data back in (frame_idx as first column)
  out <- cbind(frame_idx, df)
  
  # Combine headers + data
  out <- rbind(row1, row2, row3, as.matrix(out))
  
  # Return as character data frame
  out_df <- as.data.frame(out, stringsAsFactors = FALSE)
  names(out_df) <- NULL
  return(out_df)
}

# Function to process one file
process_file <- function(file, output_folder, scorer = "scorer") {
  fname <- basename(file)
  
  # Read depending on file type
  if (grepl("\\.csv$", file)) {
    df <- read.csv(file)
  } else if (grepl("\\.xlsx$", file)) {
    df <- read.xlsx(file)
  } else {
    message("Skipping unsupported file: ", fname)
    return(NULL)
  }
  
  # Convert
  dlc_style <- convert_to_dlc_format(df, scorer)
  
  # Build output path
  out_name <- sub("\\..*$", "_dlc.csv", fname) # force .csv
  out_path <- file.path(output_folder, out_name)
  
  # Write to CSV
  write.table(dlc_style, out_path,
              sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  message("Saved: ", out_path)
}

# Get list of files
subset_files <- list.files(input_folder, pattern = "\\.(csv|xlsx)$", full.names = TRUE)

# Process them all
lapply(subset_files, process_file, output_folder = output_folder, scorer = "")
