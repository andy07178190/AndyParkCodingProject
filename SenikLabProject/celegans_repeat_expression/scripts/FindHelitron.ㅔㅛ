import pandas as pd

# Load the bed file and count matrix
bed = pd.read_csv("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results/all_repeat_elements.bed", sep='\t', header=0)
counts = pd.read_csv("/Users/baghuijae/Desktop/Github/celegans_repeat_expression/results/all_repeat_count_matrix.csv", index_col=0)

# Expand semicolon-delimited coordinate columns
for col in ['Chr', 'Start', 'End', 'Strand']:
    bed[col] = bed[col].str.split(';')
bed_expanded = bed.explode(['Chr', 'Start', 'End', 'Strand']).copy()
bed_expanded['Start'] = bed_expanded['Start'].astype(int)
bed_expanded['End'] = bed_expanded['End'].astype(int)

# Filter for Helitron1_CE entries
mask = bed_expanded['RepeatID'].str.contains('Helitron1_CE')
helitron_positions = bed_expanded[mask]

# Merge with count data
merged = helitron_positions.merge(counts, left_on='RepeatID', right_index=True)

# Melt to long format and keep only positive counts
long = merged.melt(
    id_vars=['RepeatID', 'Chr', 'Start', 'End', 'Strand'],
    value_vars=counts.columns,
    var_name='Sample',
    value_name='Count'
)
result = long[long['Count'] > 0].sort_values(['Sample', 'Chr', 'Start'])

# Display the DataFrame
import ace_tools as tools; tools.display_dataframe_to_user(name="Helitron1_CE Positions by Sample", dataframe=result)