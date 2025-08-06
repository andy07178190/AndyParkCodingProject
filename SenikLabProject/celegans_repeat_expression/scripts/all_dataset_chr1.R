library(tidyverse)
library(scales)


results_path <- file.path(getwd(), 'results')

all_repeat_count_matrix <- read.csv(
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "all_repeat_count_matrix.csv"),
  header = TRUE
)

all_repeat_elements <- read.delim(
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "all_repeat_elements.bed"),
  sep = '\t',
  header = TRUE
)

merged_repeat_elements_and_counts <-
  all_repeat_elements %>%
  separate_longer_delim(
    cols = c(Chr, Start, End, Strand),
    delim = ';'
  ) %>%
  left_join(
    all_repeat_count_matrix,
    by = "RepeatSuperfamily"
  ) %>%
  mutate(
    Chr = as.factor(Chr),
    Start = as.numeric(Start),
    End = as.numeric(End)
  )

repeat_means_df <- merged_repeat_elements_and_counts %>%
  group_by(RepeatSuperfamily) %>%
  summarise(mean_count = mean(c_across(contains(".bam")), na.rm = TRUE))
chrI_start <- 1
chrI_stop <- 15072434
rDNA_start <- 15069280

example_plot_chrI_expression <-
  merged_repeat_elements_and_counts %>%
  filter(Chr == 'chrI') %>%
  filter(!grepl('rRNA', RepeatSuperfamily)) %>%
  select(Chr, Start, End, Strand, RepeatSuperfamily, rDNAdel.ESC134_L1_rep1__fsp_ht2.ce10_sbl_sSR.bam) %>%
  filter(rDNAdel.ESC134_L1_rep1__fsp_ht2.ce10_sbl_sSR.bam > 0) %>%
  ggplot() +
  geom_segment(
    aes(
      x = Start,
      xend = End,
      y = 0,
      yend = rDNAdel.ESC134_L1_rep1__fsp_ht2.ce10_sbl_sSR.bam
    ),
    alpha = 1
  ) +
  scale_y_sqrt() +
  scale_x_continuous(
    breaks = seq(chrI_start, chrI_stop, by = 1e6),
    labels = unit_format(unit = 'M', scale = 1e-6)
  ) +
  coord_cartesian(xlim = c(chrI_start, chrI_stop)) +
  geom_vline(
    xintercept = rDNA_start,
    color = 'red',
    linetype = 'dashed'
  ) +
  labs(
    x = 'chromosome I (bp)',
    y = 'raw counts'
  ) +
  theme_classic()

ggsave(
  filename = file.path('/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results/Chr Images', "chrI_L2FC_distribution.png"),
  plot = example_plot_chrI_expression,
  width = 10,
  height = 5
)
