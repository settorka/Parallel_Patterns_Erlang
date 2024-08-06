import subprocess
import csv
import time
import os

# Define the batch sizes
BATCH_SIZES = [10, 100, 750, 5000, 10000,
               50000, 100000, 200000, 500000, 1000000]

# Define the output CSV file
CSV_FILE = "pipeline_performance_results.csv"

# Set the working directory to the location where the Erlang modules are compiled
ERLANG_BUILD_PATH = os.path.abspath("_build\default\lib\log_analysis\ebin")  # Update this path

# Function to create the log file


def create_log_file(batch_size):
    try:
        print(f"Creating log file with batch size {batch_size}...")

        # Run the Erlang function to create the log file
        subprocess.run(
            ['erl', '-noshell', '-pa', ERLANG_BUILD_PATH, '-eval',
                f'log_file_create:create_log_file({batch_size}), erlang:halt()'],
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE,
            check=True
        )

        print(f"Log file created with batch size {batch_size}.")

    except subprocess.CalledProcessError as e:
        error_message = e.stderr.decode(
            'utf-8') if e.stderr else 'No error message'
        print(f"Error creating log file: {error_message}")

# Function to run a benchmark and capture results


def run_benchmark(batch_size, approach, module):
    try:
        print(f"Starting {approach} benchmark with batch size {batch_size}...")

        # Record the start time
        start_time = time.time()

        # Run the Erlang function
        result = subprocess.run(
            ['erl', '-noshell', '-pa', ERLANG_BUILD_PATH, '-eval',
                f'{module}:run(), erlang:halt()'],
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


# Create or overwrite the CSV file and write headers
with open(CSV_FILE, 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(["BatchSize", "Approach", "Time(sec)"])

# Iterate over batch sizes and approaches
for batch_size in BATCH_SIZES:
    create_log_file(batch_size)
    run_benchmark(batch_size, "sequential", "log_analysis_sequential")
    run_benchmark(batch_size, "parallel", "log_analysis_parallel")
    run_benchmark(batch_size, "pipeline", "log_analysis_pipeline")

print(f"Benchmarking completed. Results saved to {CSV_FILE}.")
