import pandas as pd

bed = pd.read_csv(
    "/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results/all_repeat_elements.bed",
    sep='\t',
    header=0
)
counts = pd.read_csv(
    "/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results/all_repeat_count_matrix.csv",
    index_col=0
)
for col in ['Chr', 'Start', 'End', 'Strand']:
    bed[col] = bed[col].str.split(';')
bed_expanded = bed.explode(['Chr', 'Start', 'End', 'Strand']).copy()
bed_expanded['Start'] = bed_expanded['Start'].astype(int)
bed_expanded['End']   = bed_expanded['End'].astype(int)
helitron_positions = bed_expanded[bed_expanded['RepeatID'] == 'Helitron1_CE']

merged = helitron_positions.merge(counts, left_on='RepeatID', right_index=True)

long = merged.melt(
    id_vars=['RepeatID', 'Chr', 'Start', 'End', 'Strand'],
    value_vars=counts.columns,
    var_name='Sample',
    value_name='Count'
)
result = long[long['Count'] > 0].sort_values(['Sample', 'Chr', 'Start'])

print("=== Exact Helitron1_CE Positions (first 10 rows) ===")
print(result.head(10000).to_string(index=False))

output_path = "/Users/baghuijae/Desktop/helitron1_CE_exact_positions.tsv"
result.to_csv(output_path, sep='\t', index=False)
