library(tidyverse)
library(scales)
library(ggrepel)

results_path <- file.path(getwd(), "results")

all_repeat_count_matrix <- read.csv(
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "all_repeat_count_matrix.csv"),
  header = TRUE
)

all_repeat_elements <- read.delim(
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "all_repeat_elements.bed"),
  sep = "\t",
  header = TRUE
)

merged_repeat_elements_and_counts <- 
  all_repeat_elements %>%
  # Split columns so each row has a single coordinate
  separate_longer_delim(
    cols = c(Chr, Start, End, Strand),
    delim = ";"
  ) %>%
  # Merge based on RepeatID
  left_join(all_repeat_count_matrix, by = "RepeatID") %>%
  mutate(
    Chr   = as.factor(Chr),
    Start = as.numeric(Start),
    End   = as.numeric(End)
  ) %>%
  # Calculate average raw counts for rpoa.2 and wt.N2 groups
  mutate(
    rpoa2_avg = rowMeans(select(., starts_with("rpoa.2")), na.rm = TRUE),
    N2_avg   = rowMeans(select(., starts_with("wt.N2")), na.rm = TRUE)
  ) %>%
  # Keep raw data as is, but convert N2 to negative for plotting
  mutate(
    rpoa2_raw = rpoa2_avg,
    N2_raw   = -N2_avg
  )

chrV_start <- 1
chrV_stop  <- 20924180
rpoa2_start  <- 9280923

# Filter data for chromosome II only
plot_data_chrV <- merged_repeat_elements_and_counts %>%
  filter(Chr == "chrV") %>%                  # Keep chrV
  filter(!grepl("rRNA", RepeatID)) %>%        # Exclude rRNA
  filter(rpoa2_avg > 0 & N2_avg > 0)           # Both groups must have counts > 0

# Determine the max absolute value for the y-axis, slightly expanded
max_val_chrV <- max(abs(plot_data_chrV$rpoa2_raw), abs(plot_data_chrV$N2_raw))

threshold <- 3000

# Identify repeats exceeding the threshold in both groups
label_data_chrV <- plot_data_chrV %>%
  filter(abs(rpoa2_raw) > threshold & abs(N2_raw) > threshold) %>%
  mutate(
    # Use midpoint of Start and End for the x-position of labels
    label_x = (Start + End) / 2,
    # Decide whether to place label on rpoa2_raw or N2_raw, whichever is larger in absolute value
    label_y = ifelse(abs(rpoa2_raw) > abs(N2_raw), rpoa2_raw, N2_raw)
  )

# Create the bidirectional plot for chromosome II
bidirectional_plot_chrV <- ggplot(plot_data_chrV) +
  # rpoa.2 on the top (positive)
  geom_segment(
    aes(x = Start, xend = End, y = 0, yend = rpoa2_raw),
    color = "blue", alpha = 1
  ) +
  # wt.N2 on the bottom (negative)
  geom_segment(
    aes(x = Start, xend = End, y = 0, yend = N2_raw),
    color = "black", alpha = 1
  ) +
  # x-axis: mark every 1Mb
  scale_x_continuous(
    breaks = seq(chrV_start, chrV_stop, by = 1e6),
    labels = unit_format(unit = "M", scale = 1e-6)
  ) +
  # y-axis: from -max_val_chrV to max_val_chrV, label as absolute values
  scale_y_continuous(
    name = "Raw counts",
    labels = function(x) abs(x),
    limits = c(-max_val_chrV, max_val_chrV),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  # Reference line at y=0 and rpoa2 start
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = rpoa2_start, color = "red", linetype = "dashed") +
  coord_cartesian(xlim = c(chrV_start, chrV_stop)) +
  theme_classic() +
  ggtitle("Bidirectional Expression ChrII:\n rpoa.2 (Above) vs wt.N2 (Below)") +
  geom_label_repel(
    data = label_data_chrV,
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

print(bidirectional_plot_chrV)

common_elements_chrV <- plot_data_chrV %>%
  filter(rpoa2_avg > 3000, N2_avg > 3000)

common_elements_chrV_bed <- common_elements_chrV %>%
  select(Chr, Start, End, RepeatID, Strand)

write.table(
  common_elements_chrV_bed,
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "common_elements_chrV_gt3000.bed"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = FALSE
)
