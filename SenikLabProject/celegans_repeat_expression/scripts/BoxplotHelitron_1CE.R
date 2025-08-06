# 필요한 패키지 로드
library(tidyverse)

# 1. 파일 불러오기 ---------------------------------------------------

# (a) 반복 서열의 좌표 정보 (BED 파일)
all_repeat_elements <- read.delim(
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "all_repeat_elements.bed"),
  sep = "\t",
  header = TRUE
)

# (b) 카운트 매트릭스 (CSV 파일)
all_repeat_count_matrix <- read.csv(
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "all_repeat_count_matrix.csv"),
  header = TRUE
)

# 2. 두 데이터 병합 및 Helitron 요소 필터링 ------------------------------

# 두 테이블을 RepeatID 기준 병합
merged_repeat_data <- left_join(all_repeat_elements, all_repeat_count_matrix, by = "RepeatID")

# Helitron 관련 요소만 선택 (여기선 RepeatID에 "Helitron" 문자열이 포함된 경우)
helitron_data <- merged_repeat_data %>%
  filter(grepl("Helitron", RepeatID))

# 3. 조건별(예, rDNAdel와 wt.N2) 평균 발현 계산 ---------------------------
# rDNAdel 및 wt.N2 조건의 칼럼명이 "rDNAdel"과 "wt.N2"로 시작한다고 가정

helitron_data <- helitron_data %>%
  mutate(
    # 각 행(요소)에서 해당 조건의 평균 값을 계산 (여러 샘플이 있는 경우)
    rDNA_avg = rowMeans(select(., starts_with("rDNAdel")), na.rm = TRUE),
    N2_avg   = rowMeans(select(., starts_with("wt.N2")), na.rm = TRUE)
  )

# 만약 동일한 Helitron 요소가 여러 번 기록되어 있다면 family별로 평균을 내도록 그룹화
helitron_exp_summary <- helitron_data %>%
  group_by(RepeatID) %>%
  summarize(
    rDNA_expr = mean(rDNA_avg),
    N2_expr   = mean(N2_avg)
  ) %>%
  ungroup()

# 4. 데이터를 long format으로 변환 (ggplot에서 그룹별 막대그래프 그리기 용도) ----

helitron_exp_long <- helitron_exp_summary %>%
  pivot_longer(
    cols = c(rDNA_expr, N2_expr),
    names_to = "Condition",
    values_to = "Expression"
  ) %>%
  # 조건 명칭을 보기 좋게 변경 (필요시)
  mutate(Condition = recode(Condition,
                            rDNA_expr = "rDNAdel",
                            N2_expr   = "wt.N2"))

# 5. Bar plot 생성 -----------------------------------------------------
# 각 Helitron 요소별로 조건에 따른 평균 발현을 비교하는 수평 막대그래프

bar_plot <- ggplot(helitron_exp_long, aes(x = RepeatID, y = Expression, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  coord_flip() +  # x축과 y축을 뒤집어 수평 막대로 표시
  theme_classic(base_size = 14) +
  labs(
    title = "Expression of Helitron Elements",
    subtitle = "Comparison of rDNAdel vs wt.N2",
    x = "Helitron Element",
    y = "Average Expression (counts)"
  ) +
  scale_fill_manual(values = c("rDNAdel" = "steelblue", "wt.N2" = "grey60"))

print(bar_plot)
