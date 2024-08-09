# test_performance_farm.py
import subprocess
import csv
import time
import os
from visualize_performance import create_performance_plot  # Import the new function

# Define the batch sizes
BATCH_SIZES = [10, 100, 750, 5000, 10000,
               50000, 100000, 200000, 500000, 1000000, 2000000, 4500000, 10000000, 17500000, 30000000, 50000000]

# Define the output CSV file
CSV_FILE = "farm_performance_results.csv"

# Set the working directory to the location where the Erlang modules are compiled
ERLANG_BUILD_PATH = os.path.abspath("_build/default/lib/text_write/ebin")

# Function to run a benchmark and capture results


def run_benchmark(batch_size, approach, module):
    try:
        print(f"Starting {approach} benchmark with batch size {batch_size}...")

        # Record the start time
        start_time = time.time()

        # Run the Erlang function
        result = subprocess.run(
            ['erl', '-noshell', '-pa', ERLANG_BUILD_PATH, '-eval',
                f'{module}:run({batch_size}), erlang:halt()'],
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE,
            check=True
        )

        # Record the end time
        end_time = time.time()

        # Calculate the execution time in seconds
        execution_time = end_time - start_time

        # Write the results to the CSV file
        with open(CSV_FILE, 'a', newline='') as csvfile:
            writer = csv.writer(csvfile)
            writer.writerow([batch_size, approach, execution_time])

        print(f"Completed {approach} benchmark with batch size {batch_size}.")

    except subprocess.CalledProcessError as e:
        error_message = e.stderr.decode(
            'utf-8') if e.stderr else 'No error message'
        print(f"Error running {module}: {error_message}")


def main():
    # Create or overwrite the CSV file and write headers
    with open(CSV_FILE, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(["BatchSize", "Approach", "Time(sec)"])

    # Iterate over batch sizes and approaches
    for batch_size in BATCH_SIZES:
        run_benchmark(batch_size, "sequential", "text_write_sequential")
        run_benchmark(batch_size, "parallel", "text_write_parallel")
        run_benchmark(batch_size, "farm", "text_write_farm")

    print(f"Benchmarking completed. Results saved to {CSV_FILE}.")

    # Call the visualization function
    create_performance_plot(CSV_FILE, 'performance_plot.png')


if __name__ == "__main__":
    main()
