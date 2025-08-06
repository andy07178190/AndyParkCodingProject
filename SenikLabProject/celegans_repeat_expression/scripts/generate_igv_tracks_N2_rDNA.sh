#!/bin/bash
# 스크립트를 저장한 후 실행 권한 부여 (chmod +x generate_igv_tracks_N2_rDNA.sh)하여 실행하세요.

# 파일들이 위치한 디렉토리
ALIGN_DIR="/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align"
# 생성될 IGV 트랙 파일명
OUTPUT_TRACKS_FILE="igv_tracks_N2_rDNA.txt"

# 기존 출력 파일이 있다면 삭제
if [ -f "$OUTPUT_TRACKS_FILE" ]; then
    rm "$OUTPUT_TRACKS_FILE"
fi

echo "# IGV 트랙 파일 - N2와 rDNA 샘플 비교" > "${OUTPUT_TRACKS_FILE}"
echo "# 아래 트랙 라인들을 IGV에 복사하여 붙여넣거나, 세션 파일에 포함시켜 사용하세요." >> "${OUTPUT_TRACKS_FILE}"
echo "" >> "${OUTPUT_TRACKS_FILE}"

# ALIGN_DIR 내의 모든 .bam 파일을 순회 (인덱스 .bam.bai 파일은 제외됨)
for file in "$ALIGN_DIR"/*.bam; do
    base=$(basename "$file")
    
    if [[ $base == *"wt-N2_"* ]]; then
         # N2 샘플: 파란색 (0,0,255)
         track_line="track type=bam name=\"${base%.bam}\" description=\"${base%.bam} (N2)\" bigDataUrl=${file} color=0,0,255"
         echo "${track_line}" >> "${OUTPUT_TRACKS_FILE}"
    elif [[ $base == *"rDNAdel-"* ]]; then
         # rDNA 샘플: 빨간색 (255,0,0)
         track_line="track type=bam name=\"${base%.bam}\" description=\"${base%.bam} (rDNA)\" bigDataUrl=${file} color=255,0,0"
         echo "${track_line}" >> "${OUTPUT_TRACKS_FILE}"
    fi
done

echo ""
echo "IGV 트랙 파일 생성 완료: ${OUTPUT_TRACKS_FILE}"
