import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Read the CSV file into a DataFrame
df = pd.read_csv('benchmark_results.csv')

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
plt.savefig('performance_plot.png')

# Show the plot
plt.show()
