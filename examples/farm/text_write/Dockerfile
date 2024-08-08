# official Erlang base image
FROM erlang:26

# Install rebar3 and Python
RUN apt-get update && apt-get install -y \
    rebar3 \
    python3-pip \
    && apt-get clean

# working directory set inside the container
WORKDIR /app

# current directory contents copied into the container
COPY . .

# Python dependencies for testing batch sizes and visualisation installed
RUN pip3 install -r requirements.txt

# Compile the Erlang application
RUN rebar3 compile

# Command to run the batch size performance tests and then visualize the results
CMD ["bash", "-c", "python3 test_performance_farm.py && python3 visualise_performance.py"]