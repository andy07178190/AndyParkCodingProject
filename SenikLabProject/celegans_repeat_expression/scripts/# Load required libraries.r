# Load required libraries
library(tidyverse)
library(scales)
library(DESeq2)
library(ggplot2)
library(pheatmap)
library(RColorBrewer)

results_path <- file.path(getwd(), "results")

repeat_counts <- read.csv(
  file = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results", 
                   "all_repeat_count_matrix.csv"),
  header = TRUE,
  row.names = 1
)

dim(repeat_counts)
head(repeat_counts)

repeat_counts[] <- lapply(repeat_counts, function(x) as.integer(round(as.numeric(x))))

# Create Sample Metadata
sample_names <- colnames(repeat_counts)
metadata <- data.frame(
  sample = sample_names,
  condition = ifelse(grepl("N2", sample_names), "N2", 
                     ifelse(grepl("rpoa.2.degron", sample_names), "RPOA2", "Other"))
)
rownames(metadata) <- metadata$sample
metadata

# Create DESeqDataSet and fit the model
dds <- DESeqDataSetFromMatrix(
  countData = repeat_counts,
  colData = metadata,
  design = ~ condition
)
dds <- DESeq(dds)

vsd <- varianceStabilizingTransformation(dds, blind = TRUE)
vsd_mat <- assay(vsd)

# Compute Similarity Metrics
# 1. Spearman correlation matrix (using VST-transformed counts)
spearman_corr <- cor(vsd_mat, method = "spearman")
spearman_corr[1:5, 1:5]

# 2. Euclidean distance matrix between samples
euclidean_distance <- dist(t(vsd_mat), method = "euclidean")
euclidean_matrix <- as.matrix(euclidean_distance)

# Generate Heatmaps

# Spearman correlation heatmap
pheatmap(
  spearman_corr,
  color = colorRampPalette(rev(brewer.pal(9, "RdBu")))(100),
  main = "Spearman Correlation (using DESeq2 VST Transformed)",
  clustering_method = "complete",
  fontsize_row = 7,
  fontsize_col = 7,
  angle_col = 45,
  cellwidth = 20,
  cellheight = 20,
  filename = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results",
                       "spearman_correlation_deseq2.png"),
  width = 12,
  height = 12
)

# Euclidean distance heatmap
pheatmap(
  euclidean_matrix,
  color = colorRampPalette(rev(brewer.pal(9, "RdBu")))(100),
  main = "Euclidean Distance (VST Transformed)",
  clustering_method = "ward.D2",
  fontsize_row = 8,
  fontsize_col = 8,
  angle_col = 45,
  cellwidth = 20,
  cellheight = 20,
  filename = file.path("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results",
                       "euclidean_distance_deseq2.png"),
  width = 12,
  height = 12
)
