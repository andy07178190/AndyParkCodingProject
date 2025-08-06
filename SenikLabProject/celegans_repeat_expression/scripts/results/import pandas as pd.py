import pandas as pd

# 데이터 로드
file_path = "/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results/all_repeat_count_matrix.csv"  # CSV 파일 경로
df = pd.read_csv(file_path)

# BED 형식 변환: Chromosome, Start, End 추가 (가상의 좌표 사용)
bed_data = []
start = 1000
interval = 1000  # 각 반복서열 간격 (임의 설정 가능)

for index, row in df.iterrows():
    repeat_id = row.iloc[0]
    score = row.iloc[1:].sum()  # 전체 샘플에서의 값 합산
    bed_data.append(["chr1", start, start + interval, repeat_id, score])
    start += interval

# DataFrame 생성 후 저장
bed_df = pd.DataFrame(bed_data, columns=["chrom", "start", "end", "name", "score"])
bed_df.to_csv("output.bed", sep="\t", index=False, header=False)

print("BED 파일 저장 완료: output.bed")
