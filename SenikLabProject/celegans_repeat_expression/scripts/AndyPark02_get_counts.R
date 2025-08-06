
# ==================================================================================================
# This # It also creates a bed file of repeat superfamilies, and exports a merged counts matrix.
# ==================================================================================================

# Force R to use a valid CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# - 0. Install and load required libraries -------------------------------------------------------
required_packages <- c("remotes", "powerjoin", "tidyverse")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

lapply(required_packages, install_if_missing)

# Load packages
library(tidyverse)
library(powerjoin)

# - 1. Define function to import and merge `featureCounts` outputs ---------------------------------

import_and_merge_featureCounts = function(files_to_import) {
  
  if (length(files_to_import) == 0) {
    stop
  }
  
  list_fcs = lapply(
    files_to_import,
    read.table,
    header = TRUE,
    skip = 1
  )
  
  merged_fcs =
    list_fcs %>%
    power_full_join(   
      by = c('Geneid', 'Chr', 'Start', 'End', 'Strand', 'Length')
    )
  
  return(merged_fcs)    
}

# - 2. Repeat element data processing --------------------------------------------------------------

# -- a. Define file paths to repeat element counts
counts_data_dir = "/Users/baghuijae/Desktop/Github/celegans_repeat_expression/data/counts"

# Check if directory exists
if (!dir.exists(counts_data_dir)) {
  stop(" Error: The directory 'data/counts/' does not exist.")
}

which_counts_files = list.files(
  path = counts_data_dir,
  full.names = TRUE
)

# If no files found, stop execution
if (length(which_counts_files) == 0) {
  stop(" Error: No count files found in 'data/counts/'")
}

# -- b. Import and merge repeat element counts data and genomic repeat locations

# Columns containing featureCounts metadata
bed_cols = c('Geneid', 'Chr', 'Start', 'End', 'Strand', 'Length')

# import and merge featureCounts data
repeat_counts = 
  import_and_merge_featureCounts(which_counts_files) %>%
  column_to_rownames(var = 'Geneid') %>%
  rownames_to_column(var = 'RepeatID') %>%
  dplyr::select(!any_of(bed_cols)) 

# extract bed file contents from all repeat elements
all_repeat_elements = 
  import_and_merge_featureCounts(which_counts_files) %>%
  dplyr::select(all_of(bed_cols)) %>%
  dplyr::rename(RepeatID = Geneid)

# extract repeat superfamilies with correct NA handling
repeat_superfamilies = 
  all_repeat_elements %>%
  separate(
    col = 'RepeatID',
    into = c('RepeatSuperfamily', NA),
    sep = '_dup',
    fill = "right"  
  ) %>% 
  distinct(RepeatSuperfamily)

# -- c. Summarize counts into superfamilies

repeat_superfamily_counts =
  repeat_counts %>%
  separate(
    col = 'RepeatID',
    into = c('RepeatSuperfamily', NA),
    sep = '_dup',
    fill = "right"   
  ) %>%
  group_by(RepeatSuperfamily) %>%
  summarize_all(sum) 

# -- d. Export processed data

# Create results directory if it doesn't exist
results_dir = file.path(getwd(), 'results')
if (!dir.exists(results_dir)) {
  dir.create(results_dir)
}

# Export all repeat counts matrix as .csv
write.csv(
  repeat_counts,
  file = file.path(results_dir, 'all_repeat_count_matrix.csv'),
  row.names = FALSE,
  quote = FALSE
)

# Export repeat superfamily matrix as .csv
write.csv(
  repeat_superfamily_counts,
  file = file.path(results_dir, 'repeat_superfamily_count_matrix.csv'),
  row.names = FALSE,
  quote = FALSE
)

# Export all repeat elements bed file
write.table(
  all_repeat_elements,
  file = file.path(results_dir, 'all_repeat_elements.bed'),
  row.names = FALSE,
  quote = FALSE,
  sep = '\t'
)

print("Script completed successfully! Data saved in 'results/' directory.")


