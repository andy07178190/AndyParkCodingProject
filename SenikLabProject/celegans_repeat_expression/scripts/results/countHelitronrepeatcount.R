library(dplyr)
library(readr)

# 1. CSV 파일 불러오기
data_all <- read_csv("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results/all_repeat_count_matrix.csv", 
                       show_col_types = FALSE)

# 데이터의 컬럼명을 확인하여 "RepeatSuperfamily"와 샘플 이름 확인
print(names(data_all))
head(data_all)

# Helitron 계열의 행만 선택합니다.
# 만약 해당 행의 이름이 "Helitron"이라는 단어를 포함한다면:
helitron_data <- data_all %>% 
  filter(grepl("Helitron", RepeatID))

# 헷갈리지 않도록 실제 파일에 Helitron 관련 행이 존재하는지 먼저 확인해보세요.
# 예를 들어, 헬리트론 관련 행이 없는 경우 helitron_data는 빈 데이터 프레임이 될 수 있습니다.
print(helitron_data)

# 조건별 열 선택: 
# rDNA 조건: 열 이름에 "rDNAdel"이 포함된 열
# N2 조건: 열 이름에 "wt.N2"가 포함된 열
rdna_columns <- grep("rDNAdel", names(helitron_data), value = TRUE)
n2_columns   <- grep("wt.N2", names(helitron_data), value = TRUE)

# 그룹화 후 각 그룹(Helitron 종류)별로 rDNA와 N2의 총합 계산
helitron_data_all <- helitron_data %>%
  group_by(RepeatID) %>% 
  summarise(
    rDNA_total = rowSums(across(all_of(rdna_columns)), na.rm = TRUE),
    N2_total   = rowSums(across(all_of(n2_columns)), na.rm = TRUE)
  )


# 4. 각 행(반복 요소)의 두 번째 컬럼부터 마지막 컬럼까지의 합을 Total_Count로 계산합니다.
helitron_data_all <- helitron_data_all %>% 
  mutate(Total_Count = rowSums(across(2:ncol(helitron_data_all)), na.rm = TRUE))

# 5. Total_Count 기준 내림차순 정렬 후 상위 7개 행 선택
top7_helitron_all <- helitron_data_all %>% 
  arrange(desc(Total_Count)) %>% 
  slice(1:7)

# 결과 출력
print(top7_helitron_all)

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

top7_helitron <- tibble::tribble(
  ~RepeatID,           ~rDNA_total, ~N2_total,
  "Helitron1_CE_dup11", 26257,     201,
  "Helitron1_CE",       18501,     156,
  "Helitron1_CE_dup12", 18519,     123,
  "Helitron1_CE_dup10",  7531,      72.2,
  "Helitron1_CE_dup19",  4633,      23.4,
  "Helitron1_CE_dup6",   3808,      62.1,
  "Helitron1_CE_dup57",   191,     388
)


helitron_summary <- top7_helitron %>%
  mutate(
    Total = rDNA_total + N2_total, 
    Label = paste0(RepeatID, " (n=", round(Total, 0), ")")
  ) %>%
  arrange(desc(Total)) %>%
  mutate(Label = factor(Label, levels = Label))   

# -----------------------------
helitron_long <- helitron_summary %>%
  select(Label, rDNA_total, N2_total) %>%
  pivot_longer(
    cols = c(rDNA_total, N2_total),
    names_to = "Condition",
    values_to = "Count"
  ) %>%
  mutate(
    # 이름을 간단하게 수정: rDNA_total -> "rDNA", N2_total -> "N2"
    Condition = ifelse(Condition == "rDNA_total", "rDNA", "N2")
  )

# -----------------------------
ggplot(helitron_long, aes(x = Label, y = Count, fill = Condition)) +
  geom_col(position = position_dodge()) +    
  coord_flip() +                           
  scale_fill_manual(
    values = c("rDNA" = "gray40", "N2" = "gray70")
  ) +
  labs(
    title = "Comparison of Helitron Counts in rDNA vs. N2",
    x = NULL,
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),        # 범례 제목 제거
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )
