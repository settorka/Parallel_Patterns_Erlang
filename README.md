# Multilevel Parallel Patterns

Repo for libaries implementing multilevel parallel patterns

## Project Overview
This project involves the implementation of multilevel parallel pattern libraries using Erlang. For each task, three versions will be developed:

A) Sequential Program: A baseline implementation that executes tasks sequentially.

B) Custom Parallel Program: An implementation that leverages  parallelization techniques specific to the task.

C) Custom Library Parallel Program: An implementation utilizing a custom parallel pattern library designed for this project to generalize the parallel pattern for a use case.

The primary objective is to ensure that the performance, speed, and other relevant metrics of the custom library implementation closely match those of the manually parallelized version while surpassing the sequential implementation.

### Parallel Patterns Implemented
The following parallel patterns are implemented within the custom library:

Map Pattern: Enables the parallel application of a function to a collection of elements.
Farm Pattern: Distributes tasks across multiple workers to increase throughput.
Pipeline Pattern: Organizes tasks in a sequence of stages, where each stage can be processed in parallel.

### Evaluation
To evaluate the efficacy of the custom parallel pattern libraries, two real-world tasks requiring parallelism will be implemented for each pattern. These tasks will be used to compare the performance of the sequential, manual parallel, and custom library implementations.

The project's success will be measured by the extent to which the custom library's performance approaches that of the manually parallelized version, while demonstrating significant improvements over the sequential version.

## Setup

1. Clone this repository

2. Rebar3 Setup 
### Install Rebar3 (use to install Erlang dependencies)
From Source (assuming you have a full Erlang install):
#### Windows
```bash
$ git clone https://github.com/erlang/rebar3.git
$ cd rebar3
$ ./bootstrap

enter the rebar3 directory and in the Git bash terminal, run ./bootstrap

```

#### Linux/MacOS
Latest stable compiled version:
```bash
$ wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
```

3. Install Dependencies
### Dependency setup
In the root directory, execute
```bash
$ rebar3 get-deps
$ rebar3 compile
```

4. Run a file
enter src/use_cases and enter the pattern file you want to try out
erlc <filename>
enter Erlang shell 
```bash
$ erl
```

Instructions for each pattern are in the use_cases directory Readme.md
