# ==================================================================================================
# This script merges `featureCounts` matrices from the RNA-seq analysis of 
# repeat element expression in RPOA-2 degron, rDNA deletion, and wild-type N2 strains.
# It also creates a bed file of repeat superfamilies, and exports a merged counts matrix.
# ==================================================================================================

# - 0. Load libraries ------------------------------------------------------------------------------

library(tidyverse)
library(powerjoin)


# - 1. Define function to import and merge `featureCounts` outputs ---------------------------------

import_and_merge_featureCounts = function(files_to_import) {

  list_fcs =           # define function to import `featureCounts` output files
    lapply(
      files_to_import, # list of files to import
      read.table,      # apply `read.table` to each file
      header = TRUE,   # include header: `Geneid, Chr, Start`, etc.
      skip   = 1       # skip first line (prints `featureCounts` command)
    )

  merged_fcs =
    list_fcs %>%
    power_full_join(   # full join, + append columns that don't exist (counts)
      by = c(
        'Geneid',
        'Chr',
        'Start',
        'End',
        'Strand',
        'Length'
        )
      )

  return(merged_fcs)    # return merged data frame of counts
}


# - 2. Repeat element data processing --------------------------------------------------------------

# -- a. Define file paths to repeat element counts
counts_data_dir = file.path(
  getwd(),
  'data',
  'counts'
)

which_counts_files = list.files(
  path = counts_data_dir,
  full.names = TRUE
) 


# -- b. Import and merge repeat element counts data and genomic repeat locations

# `select featureCounts` columns to be kept for peak_info dataframe;
# the remaining `featureCounts` columns are counts mapped to each sample .bam
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

# extract repeat superfamilies
repeat_superfamilies = 
  all_repeat_elements %>%
  separate(
    col = 'RepeatID',
    into = c('RepeatSuperfamily', NA),
    sep = '_dup'
  ) %>% 
  distinct(RepeatSuperfamily)


# -- c. Summarize counts into superfamilies

# summarize repeat counts into superfamilies by taking the sum of counts in each superfamily
repeat_superfamily_counts =
  repeat_counts %>%
    separate(
      col = 'RepeatID',
      into = c('RepeatSuperfamily', NA),
      sep = '_dup'
    ) %>%
    group_by(RepeatSuperfamily) %>%
    summarize_all(sum) 


# -- d. Export repeat element counts data and genomic repeat locations

# export all repeat counts matrix as .csv (for differential expression analysis, etc.)
write.csv(
  repeat_counts,
  file = file.path(
    getwd(),
    'results',
    'all_repeat_count_matrix.csv'
  ),
  row.names = FALSE,
  quote = FALSE
)


# export repeat superfamily matrix as .csv (for differential expression analysis, etc.)
write.csv(
  repeat_superfamily_counts,
  file = file.path(
    getwd(),
    'results',
    'repeat_superfamily_count_matrix.csv'
  ),
  row.names = FALSE,
  quote = FALSE
)


# export all repeat elements bed file
write.table(
  all_repeat_elements,
  file = file.path(
    getwd(),
    'results',
    'all_repeat_elements.bed'
  ),
  row.names = FALSE,
  quote = FALSE,
  sep = '\t'
)
