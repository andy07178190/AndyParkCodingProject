
#!/bin/bash

WORKDIR="/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align"
cd "$WORKDIR" || exit

mkdir -p bw

for bam in *.bam; do
    sample=$(basename "$bam" .bam)
    bamCoverage -b "$bam" -o "bw/${sample}.bw" --normalizeUsing CPM
done

wiggletools mean bw/*.bw > mean.wig

fetchChromSizes ce10 > ce10.chrom.sizes

# wig → bigWig
wigToBigWig mean.wig ce10.chrom.sizes mean.bw
