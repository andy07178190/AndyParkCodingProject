#!/bin/bash

# N2 샘플 목록
N2_SAMPLES=(
"wt-N2_L1_rep1__fsp_ht2-ce10_sbl_sSR.bw"
"wt-N2_L1_rep2__fsp_ht2-ce10_sbl_sSR.bw"
"wt-N2_L1_rep3__fsp_ht2-ce10_sbl_sSR.bw"
"wt-N2_L1_rep4__fsp_ht2-ce10_sbl_sSR.bw"
)

# rDNA 샘플 목록
rDNA_SAMPLES=(
"rDNAdel-ESC134_L1_rep1__fsp_ht2-ce10_sbl_sSR.bw"
"rDNAdel-ESC134_L1_rep2__fsp_ht2-ce10_sbl_sSR.bw"
"rDNAdel-ESC190_L1_rep1__fsp_ht2-ce10_sbl_sSR.bw"
"rDNAdel-ESC190_L1_rep2__fsp_ht2-ce10_sbl_sSR.bw"
"rDNAdel-ESC190_L1_rep3__fsp_ht2-ce10_sbl_sSR.bw"
"rDNAdel-ESC190_L1_rep4__fsp_ht2-ce10_sbl_sSR.bw"
)

# 작업 경로
BW_DIR="/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/bw"
cd "$BW_DIR" || exit 1

# 평균 계산
echo "🔄 N2 평균 계산 중..."
wiggletools mean "${N2_SAMPLES[@]}" > N2_mean.wig
wigToBigWig N2_mean.wig ce10.chrom.sizes N2_mean.bw

echo "🔄 rDNA 평균 계산 중..."
wiggletools mean "${rDNA_SAMPLES[@]}" > rDNA_mean.wig
wigToBigWig rDNA_mean.wig ce10.chrom.sizes rDNA_mean.bw

echo "🎉 완료! N2_mean.bw, rDNA_mean.bw 생성됨 🙌"
