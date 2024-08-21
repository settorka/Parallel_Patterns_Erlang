-module(farm_parallel_pattern).
-export([farm_work/4, create_or_overwrite_file/1]).

% Function to create or overwrite the file
% Parameters:
% - Filename: The name of the file to create or overwrite.
%
% Expected Output:
% - Returns 'ok' if the file is successfully created or overwritten.
% - Returns {error, Reason} if there is an error while opening the file.
%
% This function attempts to create a new file or overwrite an existing file with
% the given Filename. It opens the file in write mode. If the operation succeeds,
% the file is immediately closed to ensure it exists and is empty. If the operation
% fails, it returns an error tuple with the reason.
create_or_overwrite_file(Filename) ->
    case file:open(Filename, [write]) of
        {ok, File} ->
            file:close(File),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

% Function to split the workload and distribute it across workers
% Parameters:
% - Workload: The total amount of work (an integer) that needs to be done.
% - NumWorkers: The number of worker processes to be spawned.
% - WorkerFun: The function that each worker will execute. It takes two arguments:
%   1. Index: The worker's index (identifier).
%   2. Units: The number of work units assigned to this worker.
%
% Expected Output:
% - Returns a list of PIDs (process identifiers) for the spawned worker processes.
%
% This function divides the total Workload into chunks that are distributed among
% the specified number of worker processes. Each worker is assigned a certain number
% of units based on the division of Workload by NumWorkers. If there is a remainder,
% the first few workers handle one extra unit each. Each worker is spawned as a
% separate process running concurrently.
split_work(Workload, NumWorkers, WorkerFun) ->
    UnitsPerWorker = Workload div NumWorkers,  % Base number of units per worker
    Remainder = Workload rem NumWorkers,       % Any remaining units

    % Create and spawn worker processes
    Pids = lists:map(fun(Index) ->
        spawn(fun() ->
            % Calculate units for this worker, adding 1 if within the remainder
            WorkerUnits = UnitsPerWorker + (if Index =< Remainder -> 1; true -> 0 end),
            % Execute the worker function with the assigned number of units
            WorkerFun(Index, WorkerUnits),
            % Terminate the worker process after completion
            exit(normal)
        end)
    end, lists:seq(1, NumWorkers)),

    Pids.  % Return the list of PIDs of the spawned workers

% Wait for all worker processes to finish using process monitoring
% Parameters:
% - Pids: A list of PIDs (process identifiers) for the worker processes.
%
% Expected Output:
% - Returns 'ok' after all worker processes have exited.
%
% This function ensures that the main process waits until all spawned worker processes
% have finished their tasks. It monitors each worker process by its PID and waits
% for a 'DOWN' message, which indicates that the process has exited. This prevents
% the main process from terminating before the workers have completed their work.
wait_for_workers(Pids) ->
    % Monitor all PIDs and wait for them to exit
    lists:foreach(fun(Pid) ->
        monitor(process, Pid),  % Start monitoring the process
        receive
            {'DOWN', _Ref, process, Pid, _Reason} -> ok  % Wait for 'DOWN' message
        end
    end, Pids).

% General function to perform parallel work
% Parameters:
% - Workload: The total amount of work (an integer) that needs to be distributed.
% - NumWorkers: The number of worker processes to spawn.
% - WorkerFun: The function to be executed by each worker. It takes two arguments:
%   1. Filename: The name of the file where the worker will write its output.
%   2. Units: The number of work units assigned to this worker.
% - Filename: The name of the file that will be created or overwritten for output.
%
% Expected Output:
% - Returns 'ok' if the file is successfully created and all workers complete their tasks.
% - Returns {error, Reason} if the file operation fails.
%
% This is the main function that coordinates the parallel processing of a workload.
% It first attempts to create or overwrite the specified output file (Filename).
% If successful, it splits the workload into chunks and spawns worker processes using
% split_work/3. Each worker executes WorkerFun, handling its portion of the workload.
% After spawning the workers, the function waits for all workers to complete using
% wait_for_workers/1. Once all workers are done, it returns 'ok'.
farm_work(Workload, NumWorkers, WorkerFun, Filename) ->
    case create_or_overwrite_file(Filename) of
        ok ->
            % Split the workload among workers and spawn them
            Pids = split_work(Workload, NumWorkers, fun(_WorkerId, Units) ->
                WorkerFun(Filename, Units)
            end),
            % Wait for all workers to finish their tasks
            wait_for_workers(Pids),
            ok;
        {error, Reason} ->
            {error, Reason}  % Return error if file operation fails
    end.
