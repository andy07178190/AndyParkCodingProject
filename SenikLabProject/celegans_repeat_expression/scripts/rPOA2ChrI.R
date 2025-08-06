library(tidyverse)
library(scales)
library(ggrepel)

# Set the path to store results
results_path <- file.path(getwd(), 'results')

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
  # Split columns so that each row has a single coordinate
  separate_longer_delim(
    cols = c(Chr, Start, End, Strand),
    delim = ";"
  ) %>%
  # Merge based on RepeatID
  left_join(all_repeat_count_matrix, by = "RepeatID") %>%
  mutate(
    Chr = as.factor(Chr),
    Start = as.numeric(Start),
    End   = as.numeric(End)
  ) %>%
  # Calculate average raw counts for rpoa.2 and wt.N2 groups
  mutate(
    rpoa2_avg = rowMeans(select(., starts_with("rpoa.2")), na.rm = TRUE),
    N2_avg   = rowMeans(select(., starts_with("wt.N2")),   na.rm = TRUE)
  ) %>%
  # Keep raw data as is, convert N2 values to negative
  mutate(
    rpoa2_raw = rpoa2_avg,
    N2_raw   = -N2_avg
  )

# Define the range for chromosome I and the rpoa2 start position (UCSC reference)
chrI_start <- 1
chrI_stop  <- 15072434
rpoa2_start <- 9280923

# Extract data to be used in the plot
plot_data <- merged_repeat_elements_and_counts %>%
  filter(Chr == "chrI") %>%                      # Select chromosome I only
  filter(!grepl("rRNA", RepeatID)) %>%           # Exclude rRNA
  filter(rpoa2_avg > 0 & N2_avg > 0)              # Both groups must have counts > 0

# Determine the max absolute value for y-axis, slightly expanded for clarity
max_val <- max(abs(plot_data$rpoa2_raw), abs(plot_data$N2_raw))

# Set the threshold for labeling
threshold <- 3000

# Keep only repeat elements that exceed the threshold in both groups
label_data <- plot_data %>%
  filter(abs(rpoa2_raw) > threshold & abs(N2_raw) > threshold) %>%
  # Pre-calculate x and y positions for labeling (using the midpoint on the x-axis)
  mutate(
    label_x = (Start + End) / 2,
    label_y = ifelse(abs(rpoa2_raw) > abs(N2_raw), rpoa2_raw, N2_raw)
  )

# Create the bidirectional plot
bidirectional_plot <- ggplot(plot_data) +
  # rpoa.2 (rpoa2) on the top (positive side)
  geom_segment(
    aes(x = Start, xend = End, y = 0, yend = rpoa2_raw),
    color = "blue", alpha = 1
  ) +
  # wt.N2 on the bottom (negative side)
  geom_segment(
    aes(x = Start, xend = End, y = 0, yend = N2_raw),
    color = "black", alpha = 1
  ) +
  # x-axis: mark every 1Mb
  scale_x_continuous(
    breaks = seq(chrI_start, chrI_stop, by = 1e6),
    labels = unit_format(unit = "M", scale = 1e-6)
  ) +
  # y-axis: from -max_val to max_val, display labels as absolute values
  scale_y_continuous(
    name = "Raw counts",
    labels = function(x) abs(x),
    limits = c(-max_val, max_val),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  # Add reference line and rpoa2 start position
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = rpoa2_start, color = "red", linetype = "dashed") +
  coord_cartesian(xlim = c(chrI_start, chrI_stop)) +
  theme_classic() +
  ggtitle("Bidirectional Expression ChrI:\n rpoa.2 (Above) vs wt.N2 (Below)") +
  # Label repeat elements exceeding the threshold in both groups
  geom_label_repel(
    data = label_data,
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

print(bidirectional_plot)

# Extract elements that exceed 3000 in both groups and save to a BED file
common_elements <- merged_repeat_elements_and_counts %>%
  filter(rpoa2_avg > 3000, N2_avg > 3000)

common_elements_bed <- common_elements %>%
  select(Chr, Start, End, RepeatID, Strand)

write.table(
  common_elements_bed,
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "common_elements_gt3000.bed"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = FALSE
)
