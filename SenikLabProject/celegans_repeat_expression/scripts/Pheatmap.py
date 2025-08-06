import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

def plot_spearman_heatmap(file_path, title):
    # Load the CSV file into a DataFrame; assuming the first column contains row names
    df = pd.read_csv(file_path, index_col=0)
    
    # Optionally inspect the DataFrame
    print("DataFrame head from", file_path)
    print(df.head())
    
    # Compute the Spearman correlation matrix across the samples (columns)
    correlation_matrix = df.corr(method='spearman')
    
    # Set up the matplotlib figure
    plt.figure(figsize=(10, 8))
    
    # Create the heatmap with annotations (showing the correlation values)
    sns.heatmap(correlation_matrix, annot=True, fmt=".2f", cmap='viridis')
    
    # Add titles and labels
    plt.title(title)
    plt.xlabel("Samples")
    plt.ylabel("Samples")
    plt.tight_layout()
    
    # Display the heatmap
    plt.show()

# Example: Create a Spearman correlation heatmap for all repeat VST normalized counts.
plot_spearman_heatmap("all_repeat_count_matrix.csv", "Spearman Correlation Heatmap of VST Normalized Counts (All Repeats)")

# If you also want to create a heatmap for the repeat superfamily matrix:
plot_spearman_heatmap("repeat_superfamily_count_matrix.csv", "Spearman Correlation Heatmap of VST Normalized Counts (Repeat Superfamilies)")
