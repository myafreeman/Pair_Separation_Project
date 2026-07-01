# Combine individual dfs into a large analysis df
# add metadata (BirdID, Phase, Day, Condition)

library(dplyr)
library(readr)
library(openxlsx)

# Paths
subset_folder <- "/Users/myafreeman/Documents/GitHub/Pair_Separation_Project/Tracking_data_prep/Processed"       # where processed xlsx files live
metadata_file <- "/Users/myafreeman/Documents/GitHub/Pair_Separation_Project/SLEAP_data/metadata.csv"    # file with SessionID, Day, Condition
output_file   <- "/Users/myafreeman/Documents/GitHub/Pair_Separation_Project/Tracking_data_prep/PairSep_combined_cm.csv" # one large CSV

# Read metadata (must have columns: SessionID, Day, Condition)
metadata <- read_csv(metadata_file)

# List processed files
subset_files <- list.files(subset_folder, pattern = "\\.xlsx$", full.names = TRUE)

# Derive the stable session identifier a processed file came from, e.g.
# "bl35or15_05_05_2026_Baseline_dlc_processed.xlsx" -> "bl35or15_05_05_2026_Baseline"
get_session_id <- function(fname) {
  sub("_dlc_processed\\.xlsx$", "", fname)
}

# Function to add metadata (BirdID, Phase auto-parsed from filename; Day,
# Condition looked up from the metadata file by SessionID)
add_metadata <- function(file, metadata) {
  fname <- basename(file)
  session_id <- get_session_id(fname)

  # Read the processed tracking data
  dat <- read.xlsx(file)

  # BirdID is the first underscore-delimited token in the filename
  bird_id <- sub("_.*", "", session_id)

  # Phase is encoded in the filename as either "Baseline" or "PairSep"
  phase <- case_when(
    grepl("Baseline", session_id) ~ "Baseline",
    grepl("PairSep", session_id)  ~ "PairSep",
    TRUE ~ NA_character_
  )

  # Match metadata row for Day/Condition
  meta <- metadata %>% filter(SessionID == session_id)

  if (nrow(meta) == 0) {
    message("No metadata found for: ", session_id)
    return(NULL)
  }

  # Add metadata columns
  dat <- dat %>%
    mutate(
      BirdID = bird_id,
      Phase = phase,
      Day = meta$Day,
      Condition = meta$Condition,
      .before = 1
    )

  return(dat)
}


# Apply to all processed files and stack
all_data <- lapply(subset_files, add_metadata, metadata = metadata) %>%
  bind_rows()

# Write one large CSV file
write_csv(all_data, output_file)



