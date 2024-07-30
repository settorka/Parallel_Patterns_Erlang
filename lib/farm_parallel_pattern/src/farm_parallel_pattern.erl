-module(farm_parallel_pattern).
-export([start/3, stop/1]).

start(NumWorkers, Fun, Args) ->
    %% Create a pool of workers
    Workers = [spawn(fun() -> worker_loop(Fun) end) || _ <- lists:seq(1, NumWorkers)],
    %% Distribute tasks to workers
    lists:foreach(fun(Arg) -> distribute_task(Workers, Arg) end, Args),
    %% Wait for workers to finish
    lists:foreach(fun(Worker) -> worker_done(Worker) end, Workers).

worker_loop(Fun) ->
    receive
        {task, Arg} ->
            Fun(Arg),
            worker_loop(Fun);
        stop ->
            ok
    end.

distribute_task(Workers, Arg) ->
    %% Distribute the task to a worker
    Worker = hd(Workers),
    Worker ! {task, Arg}.

worker_done(Worker) ->
    %% Signal worker to stop
    Worker ! stop.

stop(Workers) ->
    lists:foreach(fun(Worker) -> Worker ! stop end, Workers).
