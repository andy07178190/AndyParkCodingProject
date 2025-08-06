library(tidyverse)


all_repeat_elements <- read.delim(
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "all_repeat_elements.bed"),
  sep = "\t",
  header = TRUE
)

all_repeat_count_matrix <- read.csv(
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "all_repeat_count_matrix.csv"),
  header = TRUE
)

merged_repeat_data <- left_join(all_repeat_elements, all_repeat_count_matrix, by = "RepeatID")

helitron_data <- merged_repeat_data %>%
  filter(grepl("Helitron", RepeatID))
helitron_data <- helitron_data %>%
  mutate(
    rDNA_avg = rowMeans(select(., starts_with("rDNAdel")), na.rm = TRUE),
    N2_avg   = rowMeans(select(., starts_with("wt.N2")), na.rm = TRUE)
  )

helitron_exp_summary <- helitron_data %>%
  group_by(RepeatID) %>%
  summarize(
    rDNA_expr = mean(rDNA_avg),
    N2_expr   = mean(N2_avg)
  ) %>%
  ungroup()
helitron_exp_long <- helitron_exp_summary %>%
  pivot_longer(
    cols = c(rDNA_expr, N2_expr),
    names_to = "Condition",
    values_to = "Expression"
  ) %>%
  mutate(Condition = recode(Condition,
                            rDNA_expr = "rDNAdel",
                            N2_expr   = "wt.N2"))

bar_plot <- ggplot(helitron_exp_long, aes(x = RepeatID, y = Expression, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  coord_flip() +  
  theme_classic(base_size = 14) +
  labs(
    title = "Expression of Helitron Elements",
    subtitle = "Comparison of rDNAdel vs wt.N2",
    x = "Helitron Element",
    y = "Average Expression (counts)"
  ) +
  scale_fill_manual(values = c("rDNAdel" = "steelblue", "wt.N2" = "grey60"))

print(bar_plot)
