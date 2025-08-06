# Load required libraries
library(DESeq2)    # For normalization with vst() and creating the DESeqDataSet
library(pheatmap)  # For plotting the heatmap

# 1. Read the count matrix CSV file.
count_matrix <- read.csv("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results/all_repeat_count_matrix.csv", 
                           row.names = 1)

# 2. Clean up sample names.
colnames(count_matrix) <- sub("__fsp_ht2.ce10_sbl_sSR.bam", "", colnames(count_matrix), fixed = TRUE)

# 3. Convert counts to integers.
count_matrix <- round(count_matrix)

# 4. Create a DESeqDataSet with a null design (no batch info).
dds <- DESeqDataSetFromMatrix(countData = count_matrix,
                              colData = data.frame(row.names = colnames(count_matrix)),
                              design = ~1)


# 6. Apply the variance stabilizing transformation.
vst_data <- varianceStabilizingTransformation(dds, blind = TRUE)
norm_counts <- assay(vst_data)

# 7. Compute the Pearson correlation matrix.
cor_matrix <- cor(norm_counts, method = "pearson")

# 8. Visualize the correlation matrix using a heatmap.
pheatmap(cor_matrix, main = "Correlation Heatmap")

# Load required libraries
library(DESeq2)    
library(pheatmap) 
count_matrix <- read.csv("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results/repeat_superfamily_count_matrix.csv", 
                           row.names = 1)

colnames(count_matrix) <- sub("__fsp_ht2.ce10_sbl_sSR.bam", "", colnames(count_matrix), fixed = TRUE)

# 3. Convert counts to integers.
count_matrix <- round(count_matrix)

# 4. Create a DESeqDataSet with a null design (no batch info).
dds <- DESeqDataSetFromMatrix(countData = count_matrix,
                              colData = data.frame(row.names = colnames(count_matrix)),
                              design = ~1)


# 6. Apply the variance stabilizing transformation.
vst_data <- varianceStabilizingTransformation(dds, blind = TRUE)
norm_counts <- assay(vst_data)

# 7. Compute the Pearson correlation matrix.
cor_matrix <- cor(norm_counts, method = "pearson")

# 8. Visualize the correlation matrix using a heatmap.
pheatmap(cor_matrix, main = "Correlation Heatmap")
