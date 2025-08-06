library(tidyverse)

# Set file path
file_path <- "/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results/all_repeat_elements.bed"

# Check if file exists
if (!file.exists(file_path)) {
  stop("Error: File not found! Check your file path.")
}

# Load data
data <- read.delim(file_path, sep = "\t", header = TRUE, stringsAsFactors = FALSE)

# Convert semicolon-separated values into multiple rows
expanded_data <- data %>%
  separate_rows(Chr, Start, End, Strand, sep = ";") %>%
  mutate(
    Start = as.numeric(Start),
    End = as.numeric(End),
    Chr = str_trim(Chr) # Remove extra spaces
  ) %>%
  drop_na(Start, End)  # Remove rows with missing values

# **Fix Potential Issues**
# Ensure Start < End
cleaned_data <- expanded_data %>%
  filter(Start < End)

# Remove any rows where Start or End is NA
cleaned_data <- cleaned_data %>%
  filter(!is.na(Start) & !is.na(End))

# Select relevant columns for BED format (Chr, Start, End, Name)
bed_data <- cleaned_data %>%
  select(Chr, Start, End, RepeatID)

# Ensure proper sorting
bed_data <- bed_data %>%
  arrange(Chr, Start, End)

# Save as a proper BED file (strictly tab-separated)
output_file <- file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "converted_repeat_elements.bed")

write.table(
  bed_data, 
  file = output_file, 
  sep = "\t", 
  quote = FALSE, 
  row.names = FALSE, 
  col.names = FALSE
)

print(paste("BED file saved as:", output_file))

# **Check for anomalies in the first few lines**
print(head(bed_data, 10))

