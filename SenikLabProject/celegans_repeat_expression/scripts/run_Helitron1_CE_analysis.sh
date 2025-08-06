#!/bin/bash
# Save this file as run_Helitron1_CE_analysis.sh, then give it execute permission:
#   chmod +x run_Helitron1_CE_analysis.sh

# 1. Set paths
ALIGN_DIR="/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align"
ANALYSIS_DIR="/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/analysis"
mkdir -p "$ANALYSIS_DIR"

# Path to the BED file containing Helitron1_CE region coordinates.
# Ensure that this BED file contains the correct coordinates.
HELI_BED="${ANALYSIS_DIR}/Helitron1_CE.bed"

# 2. Define your samples with complete paths.
# N2 group
N2_BAMS=(
  "${ALIGN_DIR}/wt-N2_L1_rep1__fsp_ht2-ce10_sbl_sSR.bam"
  "${ALIGN_DIR}/wt-N2_L1_rep2__fsp_ht2-ce10_sbl_sSR.bam"
  "${ALIGN_DIR}/wt-N2_L1_rep3__fsp_ht2-ce10_sbl_sSR.bam"
  "${ALIGN_DIR}/wt-N2_L1_rep4__fsp_ht2-ce10_sbl_sSR.bam"
)

# rDNA group
RDNA_BAMS=(
  "${ALIGN_DIR}/rDNAdel-ESC134_L1_rep1__fsp_ht2-ce10_sbl_sSR.bam"
  "${ALIGN_DIR}/rDNAdel-ESC134_L1_rep2__fsp_ht2-ce10_sbl_sSR.bam"
  "${ALIGN_DIR}/rDNAdel-ESC190_L1_rep1__fsp_ht2-ce10_sbl_sSR.bam"
  "${ALIGN_DIR}/rDNAdel-ESC190_L1_rep2__fsp_ht2-ce10_sbl_sSR.bam"
  "${ALIGN_DIR}/rDNAdel-ESC190_L1_rep3__fsp_ht2-ce10_sbl_sSR.bam"
  "${ALIGN_DIR}/rDNAdel-ESC190_L1_rep4__fsp_ht2-ce10_sbl_sSR.bam"
)

# 3. Generate BigWig files from each BAM file using bamCoverage.
echo "Generating BigWig files..."
# Combine both groups into one array.
all_bams=( "${N2_BAMS[@]}" "${RDNA_BAMS[@]}" )
BW_LIST=()
for bam in "${all_bams[@]}"; do
    base=$(basename "$bam" .bam)
    bw_file="${ANALYSIS_DIR}/${base}.bw"
    # Create the BigWig file if it doesn't exist.
    if [ ! -f "$bw_file" ]; then
        echo "Creating BigWig for ${base}..."
        bamCoverage -b "$bam" -o "$bw_file" --binSize 10 --normalizeUsing RPKM
    else
        echo "BigWig already exists for ${base}."
    fi
    BW_LIST+=("$bw_file")
done

# 4. Compute the coverage matrix for the Helitron1_CE region.
# In this example we use the reference-point mode with the center of the region as the reference.
MATRIX_FILE="${ANALYSIS_DIR}/Helitron1_CE_matrix.gz"
computeMatrix reference-point \
    --referencePoint center \
    -R "$HELI_BED" \
    -S "${BW_LIST[@]}" \
    --skipZeros \
    --missingDataAsZero \
    --binSize 10 \
    -o "$MATRIX_FILE"

# 5. Create a coverage profile plot.
PROFILE_PLOT="${ANALYSIS_DIR}/Helitron1_CE_profile.png"
plotProfile -m "$MATRIX_FILE" \
    -out "$PROFILE_PLOT" \
    --colors "#0000FF" "#0000FF" "#0000FF" "#0000FF" "#FF0000" "#FF0000" "#FF0000" "#FF0000" "#FF0000" "#FF0000" \
    --labels "wt-N2_rep1" "wt-N2_rep2" "wt-N2_rep3" "wt-N2_rep4" \
             "rDNAdel-ESC134_rep1" "rDNAdel-ESC134_rep2" "rDNAdel-ESC190_rep1" "rDNAdel-ESC190_rep2" "rDNAdel-ESC190_rep3" "rDNAdel-ESC190_rep4" \
    --perGroup

# 6. Optionally, create a heatmap.
HEATMAP_PLOT="${ANALYSIS_DIR}/Helitron1_CE_heatmap.png"
plotHeatmap -m "$MATRIX_FILE" -out "$HEATMAP_PLOT"

echo "Analysis complete. Please check the following outputs:"
echo "  - BigWig files in ${ANALYSIS_DIR}"
echo "  - Matrix file: $MATRIX_FILE"
echo "  - Profile plot: $PROFILE_PLOT"
echo "  - (Optional) Heatmap: $HEATMAP_PLOT"
