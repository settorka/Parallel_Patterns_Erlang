# Module to visualise performance data
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

def create_performance_plot(csv_file: str, output_file: str):
    # Read the CSV file into a DataFrame
    df = pd.read_csv(csv_file)

    # Set up the matplotlib figure
    plt.figure(figsize=(14, 8))

    # Plot the execution times for each approach with dots and lines
    sns.lineplot(data=df, x='BatchSize', y='Time(sec)',
                 hue='Approach', marker='o', linestyle='-')

    # Add title and labels
    plt.title('Execution Time by Batch Size and Approach')
    plt.xlabel('Batch Size')
    plt.ylabel('Time (seconds)')
    plt.legend(title='Approach')

    # Add grid for better readability
    plt.grid(True, which="both", linestyle="--", linewidth=0.5)

    # Save the plot as an image file
    plt.savefig(output_file)

    # Show the plot
    plt.show()
