#!/bin/bash

# .bam 파일들이 있는 곳
WORKDIR="/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align"
cd "$WORKDIR" || { echo "❌ 디렉토리 이동 실패: $WORKDIR"; exit 1; }

# bw 저장 폴더 만들기
mkdir -p bw

echo "🔄 Step 1: BAM → BW 변환 시작..."
for bam in *.bam; do
    sample=$(basename "$bam" .bam)
    echo "  📂 처리 중: $sample"
    bamCoverage -b "$bam" -o "bw/${sample}.bw" --normalizeUsing CPM
done
echo "✅ Step 1 완료!"

# 평균 bigWig 계산
echo "🔄 Step 2: 평균 계산 중..."
wiggletools mean bw/*.bw > mean.wig

# chrom.sizes 생성 (genome: ce10 기준)
echo "📏 Step 3: chrom.sizes 생성 중..."
fetchChromSizes ce10 > ce10.chrom.sizes

# mean.wig → mean.bw 변환
echo "📦 Step 4: mean.bw 생성 중..."
wigToBigWig mean.wig ce10.chrom.sizes mean.bw

echo "🎉 완료! mean.bw 파일이 생성됐어요. IGV에서 확인해봐 Hem 🙌"
