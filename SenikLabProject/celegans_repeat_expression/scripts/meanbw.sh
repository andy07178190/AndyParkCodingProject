
#!/bin/bash

# 작업 디렉토리 (bam 파일들이 있는 곳)
WORKDIR="/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align"
cd "$WORKDIR" || exit

# bw 저장 디렉토리 생성
mkdir -p bw

echo "🔄 Step 1: BAM → BW 변환 시작..."
for bam in *.bam; do
    sample=$(basename "$bam" .bam)
    echo "  📂 처리 중: $sample"
    bamCoverage -b "$bam" -o "bw/${sample}.bw" --normalizeUsing CPM
done
echo "✅ Step 1 완료: 모든 BAM → BW 변환됨"

# 평균 bigWig 만들기
echo "🔄 Step 2: 평균 bigWig 계산 중..."
wiggletools mean bw/*.bw > mean.wig

# reference genome이 ce10이라고 가정
echo "📏 Step 3: chrom.sizes 가져오기..."
fetchChromSizes ce10 > ce10.chrom.sizes

# wig → bigWig
echo "📦 Step 4: wig → bigWig 변환 중..."
wigToBigWig mean.wig ce10.chrom.sizes mean.bw
echo "✅ 평균 mean.bw 파일 생성 완료!"

echo "🎉 모든 과정 완료! IGV에서 mean.bw 파일 열어보세요 🙌"
