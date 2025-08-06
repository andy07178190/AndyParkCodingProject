# Set CRAN mirror and install necessary packages
options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
if (!requireNamespace("DESeq2", quietly = TRUE))
  BiocManager::install("DESeq2")
if (!requireNamespace("RColorBrewer", quietly = TRUE))
  install.packages("RColorBrewer")
if (!requireNamespace("pheatmap", quietly = TRUE))
  install.packages("pheatmap")

install.packages("ggrepel", repos = "https://cloud.r-project.org")
library(ggrepel)

# Load libraries
library(DESeq2)
library(ggplot2)
library(pheatmap)
library(RColorBrewer)

# Load the count matrix from CSV file (adjust the file path accordingly)
repeat_counts <- read.csv("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results/repeat_superfamily_count_matrix.csv", row.names = 1)

# Convert to a matrix and round the counts to whole numbers
repeat_counts <- as.matrix(repeat_counts)
repeat_counts <- round(repeat_counts)
mode(repeat_counts) <- "integer"

# Summarize the data and check sample names
summary(repeat_counts)
colnames(repeat_counts)

# Create metadata (each column is a sample with an associated condition)
metadata <- data.frame(
  sample = colnames(repeat_counts),
  condition = c("rDNA", "rDNA", "rDNA", "rDNA", 
                "rDNA", "rDNA", 
                "RPOA2", "RPOA2", "RPOA2", "RPOA2", "RPOA2", "RPOA2", "RPOA2", "RPOA2", 
                "N2", "N2", "N2", "N2")
)
rownames(metadata) <- metadata$sample 

# Create DESeq2 dataset and perform differential expression analysis
dds <- DESeqDataSetFromMatrix(countData = repeat_counts, colData = metadata, design = ~ condition)
dds <- DESeq(dds)

# Calculate contrasts (comparison results)
# Store the original DESeqResults object (for example, for plotMA)
res_rDNA_N2_orig <- results(dds, contrast = c("condition", "rDNA", "N2"))
# Order results by adjusted p-value and convert to a data frame for downstream analysis
res_rDNA_N2 <- as.data.frame(res_rDNA_N2_orig[order(res_rDNA_N2_orig$padj, na.last = NA), ])

res_RPOA2_Control_orig <- results(dds, contrast = c("condition", "RPOA2", "N2"))
res_RPOA2_Control <- as.data.frame(res_RPOA2_Control_orig[order(res_RPOA2_Control_orig$padj, na.last = NA), ])

res_RPOA2_rDNA_orig <- results(dds, contrast = c("condition", "RPOA2", "rDNA"))
res_RPOA2_rDNA <- as.data.frame(res_RPOA2_rDNA_orig[order(res_RPOA2_rDNA_orig$padj, na.last = NA), ])


# Set the significance threshold for differential expression
deg_threshold <- 0.05

# Subset significant genes for rDNA vs. N2 (padj < threshold and absolute log2FoldChange > 1)
sig_rDNA_N2 <- subset(res_rDNA_N2, padj < deg_threshold & abs(log2FoldChange) > 1)

# Create a volcano plot for rDNA vs. N2 with significant points highlighted and labeled
volcano_rDNA_N2 <- ggplot(res_rDNA_N2, aes(x = log2FoldChange, y = -log10(padj + 1e-10))) +
  geom_point(alpha = 0.7) +  # Plot all points
  geom_point(data = sig_rDNA_N2, aes(x = log2FoldChange, y = -log10(padj + 1e-10)), 
             color = "red", size = 2) +  # Highlight significant points in red
  geom_text_repel(data = sig_rDNA_N2, 
                  aes(x = log2FoldChange, y = -log10(padj + 1e-10), label = rownames(sig_rDNA_N2)),
                  size = 3, max.overlaps = 20) +  # Label significant points
  theme_minimal() +
  labs(
    title = "Volcano Plot for Differential Expression (log2 fold change vs. statistical significance): rDNA vs. N2",
    subtitle = paste("Number of DEGs:", nrow(sig_rDNA_N2)),
    x = "Log2 Fold Change (rDNA vs. N2)",
    y = "-Log10 Adjusted P-value"
  ) +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "blue") +  
  geom_hline(yintercept = -log10(deg_threshold), linetype = "dashed", color = "red")  
print(volcano_rDNA_N2)

sig_RPOA2_N2 <- subset(res_RPOA2_Control, padj < deg_threshold & abs(log2FoldChange) > 1)
volcano_RPOA2_N2 <- ggplot(res_RPOA2_Control, aes(x = log2FoldChange, y = -log10(padj + 1e-10))) +
  geom_point(alpha = 0.7) +  # Plot all points
  geom_point(data = sig_RPOA2_N2, aes(x = log2FoldChange, y = -log10(padj + 1e-10)),
             color = "red", size = 2) +  # Highlight significant points in red
  geom_text_repel(data = sig_RPOA2_N2,
                  aes(x = log2FoldChange, y = -log10(padj + 1e-10), label = rownames(sig_RPOA2_N2)),
                  size = 3, max.overlaps = 20) +  # Label significant points
  theme_minimal() +
  labs(
    title = "Volcano Plot: RPOA2 Depletion vs. N2",
    subtitle = paste("Number of DEGs:", nrow(sig_RPOA2_N2)),
    x = "Log2 Fold Change (RPOA2 vs. N2)",
    y = "-Log10 Adjusted P-value"
  ) +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "blue") +  # Add vertical threshold lines
  geom_hline(yintercept = -log10(deg_threshold), linetype = "dashed", color = "red")  # Add horizontal threshold line

print(volcano_RPOA2_N2)
sessionInfo()
