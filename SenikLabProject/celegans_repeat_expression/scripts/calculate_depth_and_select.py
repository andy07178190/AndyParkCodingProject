#!/usr/bin/env python3
import subprocess
import statistics
import csv
import os

# Calculate average depth of a BAM file using samtools and awk
def get_avg_depth_with_depth_cmd(bam_file):
    """
    Calculates the average depth of a BAM file using 'samtools depth'.
    :param bam_file: Full path to the input BAM file
    :return: Average depth (float), or None if calculation fails
    """
    cmd = f"samtools depth {bam_file} | awk '{{sum+=$3; count++}} END {{if (count>0) print sum/count; else print 0}}'"
    try:
        result = subprocess.check_output(cmd, shell=True, text=True)
        avg_depth = float(result.strip())
        return avg_depth
    except subprocess.CalledProcessError as e:
        print(f"Command error for {bam_file}: {e}")
    except ValueError as ve:
        print(f"Parsing error for {bam_file}: {ve}")
    return None

# Write results into a CSV file
def write_csv(data, csv_file):
    """
    Writes depth data into a CSV file.
    :param data: List of dictionaries [{'sample':str, 'group':str, 'avg_depth':float}, ...]
    :param csv_file: Output CSV file path
    """
    with open(csv_file, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["sample", "group", "avg_depth"])
        writer.writeheader()
        for row in data:
            writer.writerow(row)

# Find the representative sample closest to the group mean
def get_representative_sample(sample_data, group_mean):
    """
    Finds the sample closest to the group's mean depth.
    :param sample_data: List of dictionaries [{'sample':str, 'avg_depth':float}, ...]
    :param group_mean: Mean depth value of the group
    :return: Path to the sample with depth closest to group mean
    """
    best_sample = None
    best_diff = float("inf")
    for item in sample_data:
        diff = abs(item["avg_depth"] - group_mean)
        if diff < best_diff:
            best_diff = diff
            best_sample = item["sample"]
    return best_sample

# Main analysis workflow
def main():
    # Output CSV file path
    output_dir = "/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results"
    os.makedirs(output_dir, exist_ok=True)
    csv_filename = os.path.join(output_dir, "depth_statistics.csv")
    all_results = []

    # List of N2 group BAM file paths
    n2_samples = [
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/wt-N2_L1_rep1__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/wt-N2_L1_rep2__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/wt-N2_L1_rep3__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/wt-N2_L1_rep4__fsp_ht2-ce10_sbl_sSR.bam",
    ]

    # List of rDNA group BAM file paths
    rdna_samples = [
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC134_L1_rep1__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC134_L1_rep2__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC190_L1_rep1__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC190_L1_rep2__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC190_L1_rep3__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC190_L1_rep4__fsp_ht2-ce10_sbl_sSR.bam",
    ]

    # Calculate depth for N2 samples
    n2_depths_data = []
    for bam in n2_samples:
        depth = get_avg_depth_with_depth_cmd(bam)
        if depth is not None:
            n2_depths_data.append({"sample": bam, "avg_depth": depth})
            all_results.append({"sample": bam, "group": "N2", "avg_depth": depth})

    # Calculate depth for rDNA samples
    rdna_depths_data = []
    for bam in rdna_samples:
        depth = get_avg_depth_with_depth_cmd(bam)
        if depth is not None:
            rdna_depths_data.append({"sample": bam, "avg_depth": depth})
            all_results.append({"sample": bam, "group": "rDNA", "avg_depth": depth})

    # Write results to CSV
    write_csv(all_results, csv_filename)
    print(f"\nResults saved to '{csv_filename}'.")

    # Calculate and print summary statistics (mean and standard deviation)
    if n2_depths_data:
        n2_mean = statistics.mean([x["avg_depth"] for x in n2_depths_data])
        n2_stdev = statistics.stdev([x["avg_depth"] for x in n2_depths_data])
        n2_repr = get_representative_sample(n2_depths_data, n2_mean)
        print(f"[N2] Mean = {n2_mean}, Std Dev = {n2_stdev}\nRepresentative sample: {n2_repr}")

    if rdna_depths_data:
        rdna_mean = statistics.mean([x["avg_depth"] for x in rdna_depths_data])
        rdna_stdev = statistics.stdev([x["avg_depth"] for x in rdna_depths_data])
        rdna_repr = get_representative_sample(rdna_depths_data, rdna_mean)
        print(f"[rDNA] Mean = {rdna_mean}, Std Dev = {rdna_stdev}\nRepresentative sample: {rdna_repr}")

if __name__ == "__main__":
    main()