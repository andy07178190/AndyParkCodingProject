library(tidyverse)
library(scales)
library(ggrepel)

# Set the path to store results
results_path <- file.path(getwd(), "results")

# Load the count matrix
all_repeat_count_matrix <- read.csv(
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "all_repeat_count_matrix.csv"),
  header = TRUE
)

# Load repeat elements' coordinate information
all_repeat_elements <- read.delim(
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "all_repeat_elements.bed"),
  sep = "\t",
  header = TRUE
)

# Merge and preprocess repeat elements and count matrix
merged_repeat_elements_and_counts <- 
  all_repeat_elements %>%
  # Separate genomic coordinates so that each row has only one coordinate
  separate_longer_delim(
    data = .,
    cols = c(Chr, Start, End, Strand),
    delim = ";"
  ) %>%
  # Join with the repeat count matrix using 'RepeatID'
  left_join(
    .,
    all_repeat_count_matrix,
    by = "RepeatID"
  ) %>%
  dplyr::mutate(
    Chr = as.factor(Chr),
    Start = as.numeric(Start),
    End = as.numeric(End)
  ) %>%
  # Calculate average raw counts for rDNAdel and wt.N2 groups
  mutate(
    rDNA_avg = rowMeans(select(., starts_with("rDNAdel")), na.rm = TRUE),
    N2_avg   = rowMeans(select(., starts_with("wt.N2")), na.rm = TRUE)
  ) %>%
  # Keep raw data as is, but convert N2 to negative for plotting
  mutate(
    rDNA_raw = rDNA_avg,
    N2_raw   = -N2_avg
  )

# Set chromosome IV boundaries (from UCSC Genome Browser)
chrIV_start <- 1
chrIV_stop  <- 17493829
rDNA_start  <- 15069280

# Filter data for chromosome IV only
plot_data_chrIV <- merged_repeat_elements_and_counts %>%
  filter(Chr == "chrIV") %>%                  # Keep chrIV
  filter(!grepl("rRNA", RepeatID)) %>%         # Exclude rRNA
  filter(rDNA_avg > 0 & N2_avg > 0)             # Both groups must have counts > 0

# Determine the max absolute value for the y-axis, slightly expanded
max_val_chrIV <- max(abs(plot_data_chrIV$rDNA_raw), abs(plot_data_chrIV$N2_raw))

# Set the threshold for labeling (e.g., 3000)
threshold <- 3000

# Identify repeats exceeding the threshold in both groups
label_data_chrIV <- plot_data_chrIV %>%
  filter(abs(rDNA_raw) > threshold | abs(N2_raw) > threshold) %>%
  mutate(
    # Use the midpoint of Start and End for the x-position of labels
    label_x = (Start + End) / 2,
    # Place the label at the value (rDNA_raw or N2_raw) with the larger absolute value
    label_y = ifelse(abs(rDNA_raw) > abs(N2_raw), rDNA_raw, N2_raw)
  )

# Create the bidirectional plot for chromosome IV
bidirectional_plot_chrIV <- ggplot(plot_data_chrIV) +
  # rDNAdel on the top (positive)
  geom_segment(
    aes(x = Start, xend = End, y = 0, yend = rDNA_raw),
    color = "blue", alpha = 1
  ) +
  # wt.N2 on the bottom (negative)
  geom_segment(
    aes(x = Start, xend = End, y = 0, yend = N2_raw),
    color = "black", alpha = 1
  ) +
  # x-axis: mark every 1Mb
  scale_x_continuous(
    breaks = seq(chrIV_start, chrIV_stop, by = 1e6),
    labels = unit_format(unit = "M", scale = 1e-6)
  ) +
  # y-axis: from -max_val_chrIV to max_val_chrIV, label as absolute values
  scale_y_continuous(
    name = "Raw counts",
    labels = function(x) abs(x),
    limits = c(-max_val_chrIV, max_val_chrIV),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  # Add reference lines at y=0 and the rDNA start position
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = rDNA_start, color = "red", linetype = "dashed") +
  coord_cartesian(xlim = c(chrIV_start, chrIV_stop)) +
  theme_classic() +
  ggtitle("Bidirectional Expression ChrIV:\n rDNAdel (Above) vs wt.N2 (Below)") +
  # Label the repeats that exceed the threshold in both groups
  geom_label_repel(
    data = label_data_chrIV,
    aes(
      x = label_x,
      y = label_y,
      label = RepeatID
    ),
    size = 3,
    angle = 0,
    nudge_y = 300,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.alpha = 0.5,
    max.overlaps = Inf
  )

# Display the plot
print(bidirectional_plot_chrIV)

# Extract elements on chrIV that exceed 3000 in both groups, then write to BED
common_elements_chrIV <- plot_data_chrIV %>%
  filter(rDNA_avg > 3000, N2_avg > 3000)

common_elements_chrIV_bed <- common_elements_chrIV %>%
  select(Chr, Start, End, RepeatID, Strand)

write.table(
  common_elements_chrIV_bed,
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "common_elements_chrIV_gt3000.bed"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = FALSE
)
