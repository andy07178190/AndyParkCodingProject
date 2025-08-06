library(tidyverse)
library(scales)


# 결과 파일 저장 경로 설정
results_path <- file.path(getwd(), 'results')

# count matrix 불러오기 (헤더에 RepeatSuperfamily 포함)
all_repeat_count_matrix <- read.csv(
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "all_repeat_count_matrix.csv"),
  header = TRUE
)

# repeat elements BED 파일 불러오기
all_repeat_elements <- read.delim(
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", "all_repeat_elements.bed"),
  sep = '\t',
  header = TRUE
)

# repeat elements와 count matrix 결합
merged_repeat_elements_and_counts <-
  all_repeat_elements %>%
  # 여러 genomic coordinate이 한 행에 있을 경우 각 coordinate을 별도의 행으로 분리
  separate_longer_delim(
    cols = c(Chr, Start, End, Strand),
    delim = ';'
  ) %>%
  # RepeatSuperfamily 열을 기준으로 결합 (count matrix와 BED 파일 모두 해당 열을 포함해야 함)
  left_join(
    all_repeat_count_matrix,
    by = "RepeatSuperfamily"
  ) %>%
  mutate(
    Chr = as.factor(Chr),
    Start = as.numeric(Start),
    End = as.numeric(End)
  )

# (필요시) RepeatSuperfamily별 모든 .bam 컬럼의 평균값 계산
repeat_means_df <- merged_repeat_elements_and_counts %>%
  group_by(RepeatSuperfamily) %>%
  summarise(mean_count = mean(c_across(contains(".bam")), na.rm = TRUE))

# UCSC genome browser에서 가져온 chr I 범위 설정
chrI_start <- 1
chrI_stop <- 15072434
rDNA_start <- 15069280

# chr I 상의 repeat element expression 예시 플롯 (기존과 동일한 포맷)
example_plot_chrI_expression <-
  merged_repeat_elements_and_counts %>%
  # chrI만 선택 (rDNA가 chrI에 위치)
  filter(Chr == 'chrI') %>%
  # rRNA 요소 제거 (read count가 너무 높아 다른 요소를 보기 어려울 경우)
  filter(!grepl('rRNA', RepeatSuperfamily)) %>%
  # 예시로 하나의 샘플(rDNAdel.ESC134_L1_rep1__fsp_ht2.ce10_sbl_sSR.bam)만 선택
  select(Chr, Start, End, Strand, RepeatSuperfamily, rDNAdel.ESC134_L1_rep1__fsp_ht2.ce10_sbl_sSR.bam) %>%
  # 해당 샘플의 count가 0보다 큰 데이터만 사용
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

# 플롯 이미지 저장
ggsave(
  filename = file.path('/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results/Chr Images', "chrI_L2FC_distribution.png"),
  plot = example_plot_chrI_expression,
  width = 10,
  height = 5
)
