#!/bin/bash

ALIGN_DIR="/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align"
OUTPUT_TRACKS_FILE="igv_tracks_N2_rDNA.txt"

if [ -f "$OUTPUT_TRACKS_FILE" ]; then
    rm "$OUTPUT_TRACKS_FILE"
fi


echo "" >> "${OUTPUT_TRACKS_FILE}"

for file in "$ALIGN_DIR"/*.bam; do
    base=$(basename "$file")
    
    if [[ $base == *"wt-N2_"* ]]; then
         # N2 : Blue
         track_line="track type=bam name=\"${base%.bam}\" description=\"${base%.bam} (N2)\" bigDataUrl=${file} color=0,0,255"
         echo "${track_line}" >> "${OUTPUT_TRACKS_FILE}"
    elif [[ $base == *"rDNAdel-"* ]]; then
         # rDNA : Red
         track_line="track type=bam name=\"${base%.bam}\" description=\"${base%.bam} (rDNA)\" bigDataUrl=${file} color=255,0,0"
         echo "${track_line}" >> "${OUTPUT_TRACKS_FILE}"
    fi
done

echo "done"