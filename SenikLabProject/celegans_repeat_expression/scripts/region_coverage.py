#!/usr/bin/env python3
import subprocess

def calculate_region_coverage(bam_file, region):
    cmd = f"samtools depth -r {region} {bam_file} | awk '{{sum+=$3}} END {{if (NR>0) print sum/NR; else print 0}}'"
    try:
        result = subprocess.check_output(cmd, shell=True, text=True)
        return float(result.strip())
    except subprocess.CalledProcessError as e:
        print(f"Error processing {bam_file}: {e}")
        return None
    except ValueError as ve:
        print(f"Parsing error for {bam_file}: {ve}")
        return None

def main():
    region = "chrI:1015548-1018833"
    
    samples = {
        "N2": "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/wt-N2_L1_rep3__fsp_ht2-ce10_sbl_sSR.bam",
        "rDNA_del": "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC190_L1_rep3__fsp_ht2-ce10_sbl_sSR.bam"
    }
    
    print(f"Coverage calculation for region: {region}\n")
    for group, bam in samples.items():
        coverage = calculate_region_coverage(bam, region)
        if coverage is not None:
            print(f"{group}: average coverage = {coverage:.2f}")
        else:
            print(f"{group}: calculation failed")

if __name__ == "__main__":
    main()
