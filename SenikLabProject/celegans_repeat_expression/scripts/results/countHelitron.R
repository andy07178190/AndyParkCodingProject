# 필요한 라이브러리 로드
library(dplyr)
library(readr)

# CSV 파일 불러오기 (파일 경로를 실제 경로로 변경)
data <- read_csv("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results/repeat_superfamily_count_matrix.csv", show_col_types = FALSE)

# 데이터의 컬럼명을 확인하여 "RepeatSuperfamily"와 샘플 이름 확인
print(names(data))
head(data)

# Helitron 계열의 행만 선택합니다.
# 만약 해당 행의 이름이 "Helitron"이라는 단어를 포함한다면:
helitron_data <- data %>% 
  filter(grepl("Helitron", RepeatSuperfamily))

# 헷갈리지 않도록 실제 파일에 Helitron 관련 행이 존재하는지 먼저 확인해보세요.
# 예를 들어, 헬리트론 관련 행이 없는 경우 helitron_data는 빈 데이터 프레임이 될 수 있습니다.
print(helitron_data)

# 조건별 열 선택: 
# rDNA 조건: 열 이름에 "rDNAdel"이 포함된 열
# N2 조건: 열 이름에 "wt.N2"가 포함된 열
rdna_columns <- grep("rDNAdel", names(helitron_data), value = TRUE)
n2_columns   <- grep("wt.N2", names(helitron_data), value = TRUE)

# 그룹화 후 각 그룹(Helitron 종류)별로 rDNA와 N2의 총합 계산
helitron_summary <- helitron_data %>%
  group_by(RepeatSuperfamily) %>% 
  summarise(
    rDNA_total = rowSums(across(all_of(rdna_columns)), na.rm = TRUE),
    N2_total   = rowSums(across(all_of(n2_columns)), na.rm = TRUE)
  )

print(helitron_summary)

library(ggplot2)
library(tidyr)
library(dplyr)

# -----------------------------
# (1) helitron_summary 예시
# -----------------------------
helitron_summary <- tibble::tribble(
  ~RepeatSuperfamily, ~rDNA_total, ~N2_total,
  "Helitron1_CE",   79995, 1552,
  "Helitron2_CE",    1881, 1006,
  "HelitronY1A_CE",  2375, 1843,
  "HelitronY1_CE",    919,  711,
  "HelitronY2_CE",    322,  585,
  "HelitronY3_CE",    183,  129,
  "HelitronY4_CE",   1078,  869
)

# (총합 계산) 각 Helitron의 총 카운트: (rDNA_total + N2_total)
helitron_summary <- helitron_summary %>%
  mutate(Total = rDNA_total + N2_total)

# (Helitron 라벨 생성) 예: Helitron1_CE (n=81547)
helitron_summary <- helitron_summary %>%
  mutate(
    Label = paste0(RepeatSuperfamily, " (n=", round(Total, 0), ")")
  )

# (정렬) 만약 Total이 큰 순서대로 위에서부터 보이게 하고 싶다면,
# desc(Total)로 정렬 후, factor 레벨 지정
helitron_summary <- helitron_summary %>%
  arrange(desc(Total)) %>%
  mutate(Label = factor(Label, levels = Label))

# -----------------------------
# (2) long format 변환
# -----------------------------
helitron_long <- helitron_summary %>%
  select(Label, rDNA_total, N2_total) %>%
  pivot_longer(
    cols = c(rDNA_total, N2_total),
    names_to = "Condition",
    values_to = "Count"
  )

# -----------------------------
# (3) 그래프 생성
# -----------------------------
ggplot(helitron_long, aes(x = Label, y = Count, fill = Condition)) +
  geom_col(position = position_dodge()) +    # 막대를 나란히 표시
  coord_flip() +                             # 가로 방향으로 뒤집기
  # 색상 수동 지정 (원하는 색상으로 교체 가능)
  scale_fill_manual(
    values = c("rDNA_total" = "gray40", "N2_total" = "gray70"),
    labels = c("rDNA_total" = "rDNA", "N2_total" = "N2")
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

