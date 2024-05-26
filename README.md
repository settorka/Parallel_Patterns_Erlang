# Multilevel Parallel Patterns

Repo for libaries implementing multilevel parallel patterns

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
