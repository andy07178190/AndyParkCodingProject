#!/bin/bash

WORKDIR="/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align"
cd "$WORKDIR" || { echo "❌ 디렉토리 이동 실패: $WORKDIR"; exit 1; }

mkdir -p bw

for bam in *.bam; do
    sample=$(basename "$bam" .bam)
    echo "  📂 처리 중: $sample"
    bamCoverage -b "$bam" -o "bw/${sample}.bw" --normalizeUsing CPM
done

wiggletools mean bw/*.bw > mean.wig

# chrom.sizes 
fetchChromSizes ce10 > ce10.chrom.sizes

wigToBigWig mean.wig ce10.chrom.sizes mean.bw

echo "done"
