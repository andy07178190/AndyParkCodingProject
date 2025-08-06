#!/usr/bin/env python3
import subprocess
import statistics

def get_average_depth(bam_file):
    """
    samtools stats를 실행하여 BAM 파일의 평균 깊이(coverage)를 추출합니다.
    출력 형식은 samtools 버전에 따라 다를 수 있으므로, 필요에 따라 파싱 부분을 수정하세요.
    """
    cmd = ["samtools", "stats", bam_file]
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
    except subprocess.CalledProcessError as e:
        print(f"Error running samtools stats on {bam_file}: {e}")
        return None

    avg_depth = None
    # samtools stats의 출력 중 "SN"으로 시작하며 "average depth:" 라인이 평균 깊이를 포함합니다.
    for line in result.stdout.splitlines():
        if line.startswith("SN") and "average depth:" in line:
            # 일반적인 출력 형식: "SN\taverage depth:\t<값>"
            parts = line.split("\t")
            try:
                # parts[2]에 평균 값이 있을 것으로 가정함
                avg_depth = float(parts[2].strip())
            except (IndexError, ValueError):
                print(f"Parsing error for line: {line}")
            break
    return avg_depth

def main():
    # N2 그룹 BAM 파일 전체 경로
    n2_samples = [
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/wt-N2_L1_rep1__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/wt-N2_L1_rep2__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/wt-N2_L1_rep3__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/wt-N2_L1_rep4__fsp_ht2-ce10_sbl_sSR.bam",
    ]
    
    # rDNA 그룹 BAM 파일 전체 경로
    rdna_samples = [
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC134_L1_rep1__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC134_L1_rep2__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC190_L1_rep1__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC190_L1_rep2__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC190_L1_rep3__fsp_ht2-ce10_sbl_sSR.bam",
        "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/rDNAdel-ESC190_L1_rep4__fsp_ht2-ce10_sbl_sSR.bam",
    ]
    
    n2_depths = []
    print("=== N2 샘플 평균 깊이 계산 ===")
    for bam in n2_samples:
        depth = get_average_depth(bam)
        if depth is not None:
            print(f"{bam} -> average depth = {depth}")
            n2_depths.append(depth)
        else:
            print(f"{bam} -> 평균 깊이 계산 실패")

    rdna_depths = []
    print("\n=== rDNA 샘플 평균 깊이 계산 ===")
    for bam in rdna_samples:
        depth = get_average_depth(bam)
        if depth is not None:
            print(f"{bam} -> average depth = {depth}")
            rdna_depths.append(depth)
        else:
            print(f"{bam} -> 평균 깊이 계산 실패")
    
    # 각 그룹의 평균과 표준편차 계산
    if n2_depths:
        n2_mean = statistics.mean(n2_depths)
        n2_stdev = statistics.stdev(n2_depths) if len(n2_depths) > 1 else 0
        print(f"\n[N2 그룹] 평균 = {n2_mean}, 표준편차 = {n2_stdev}")
    else:
        print("\nN2 그룹의 데이터가 없습니다.")
    
    if rdna_depths:
        rdna_mean = statistics.mean(rdna_depths)
        rdna_stdev = statistics.stdev(rdna_depths) if len(rdna_depths) > 1 else 0
        print(f"[rDNA 그룹] 평균 = {rdna_mean}, 표준편차 = {rdna_stdev}")
    else:
        print("rDNA 그룹의 데이터가 없습니다.")

if __name__ == "__main__":
    main()
