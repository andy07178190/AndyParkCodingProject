#!/usr/bin/env python3
import subprocess
import statistics

def get_average_depth(bam_file):
    cmd = ["samtools", "stats", bam_file]
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
    except subprocess.CalledProcessError as e:
        print(f"Error running samtools stats on {bam_file}: {e}")
        return None

    avg_depth = None
    for line in result.stdout.splitlines():
        if line.startswith("SN") and "average depth:" in line:
                    parts = line.split("\t")
            try:
                avg_depth = float(parts[2].strip())
            except (IndexError, ValueError):
                print(f"Parsing error for line: {line}")
            break
    return avg_depth

def main():
    n2_samples = [
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/wt-N2_L1_rep1__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/wt-N2_L1_rep2__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/wt-N2_L1_rep3__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/wt-N2_L1_rep4__fsp_ht2-ce10_sbl_sSR.bam",
    ]
    rdna_samples = [
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC134_L1_rep1__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC134_L1_rep2__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC190_L1_rep1__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC190_L1_rep2__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC190_L1_rep3__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC190_L1_rep4__fsp_ht2-ce10_sbl_sSR.bam",
    ]
    
    n2_depths = []
    for bam in n2_samples:
        depth = get_average_depth(bam)
        if depth is not None:
            n2_depths.append(depth)
        else:

    rdna_depths = []
    for bam in rdna_samples:
        depth = get_average_depth(bam)
        if depth is not None:
            rdna_depths.append(depth)
        else:
        
    if n2_depths:
        n2_mean = statistics.mean(n2_depths)
        n2_stdev = statistics.stdev(n2_depths) if len(n2_depths) > 1 else 0
    else:
    
    if rdna_depths:
        rdna_mean = statistics.mean(rdna_depths)
        rdna_stdev = statistics.stdev(rdna_depths) if len(rdna_depths) > 1 else 0
    else:

if __name__ == "__main__":
    main()
