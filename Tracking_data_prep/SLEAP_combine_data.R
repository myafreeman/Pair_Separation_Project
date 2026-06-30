# Combine individual dfs into a large analysis df 
# add metadata (condition, playbacks...)


# Paths
subset_folder <- ""       # where subsetted xlsx files live
metadata_file <- ""      # file with BirdID, Batch, etc.
output_file   <- "path/UnmanipulatedBirds_combined_cm.csv" # one large CSV

# Read metadata
metadata <- read_csv(metadata_file)

# List subsetted files
subset_files <- list.files(subset_folder, pattern = "\\.xlsx$", full.names = TRUE)

# MANIPULATED Function to add metadata for injected birds
add_metadata <- function(file, metadata) {
  fname <- basename(file)
  
  # Read the subsetted tracking data
  dat <- read.xlsx(file)
  
  # Recover original filename (before "_intervalX.xlsx")
  #base_name <- sub("_interval.*\\.xlsx$", ".csv", fname) ###Change for changing cleaned data back to original
  
  # Match metadata row
  meta <- metadata %>% filter(file == fname)
  
  if (nrow(meta) == 0) {
    message("No metadata found for: ", fname)
    return(NULL)
  }
  
  # Add metadata columns
  dat <- dat %>%
    mutate(
      BirdID = meta$BirdID,
      Batch = meta$Batch,
      Condition = meta$Condition,
      Injection = meta$Injection,
      Concentration = meta$Concentration,
      Lesion = meta$Lesion,
      .before = 1
    )
  
  return(dat)
}


# UNMANIPULATED Function to add metadata for unmanipulated birds 
add_metadata <- function(file, metadata) {
  fname <- basename(file)
  
  # Read the subsetted tracking data
  dat <- read.xlsx(file)
  
  # Recover original filename (before "_intervalX.xlsx")
  #base_name <- sub("_interval.*\\.xlsx$", ".csv", fname) ###ChANGE for changing cleaned data back to original
  
  # Match metadata row
  meta <- metadata %>% filter(file == fname)
  
  if (nrow(meta) == 0) {
    message("No metadata found for: ", fname)
    return(NULL)
  }
  
  # Add metadata columns
  dat <- dat %>%
    mutate(
      BirdID = meta$BirdID,
      Condition = meta$Condition,
      Playbacks = meta$Playbacks,
      .before = 1
    )
  
  return(dat)
}


# Apply to all subsetted files and stack
all_data <- lapply(subset_files, add_metadata, metadata = metadata) %>%
  bind_rows()

# Write one large CSV file
write_csv(all_data, output_file)



