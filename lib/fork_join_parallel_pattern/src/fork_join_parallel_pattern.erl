-module(fork_join_parallel_pattern).
-export([process/2, fork/2, join/1]).

% Main function to process a given function and its arguments using fork-join pattern
process(Fun, Args) ->
    NumThreads = erlang:system_info(schedulers_online),
    Pids = fork(Fun, chunk_args(Args, NumThreads)),
    join(Pids).

% Fork function to create parallel tasks
fork(Fun, Chunks) ->
    lists:map(
        fun(Chunk) ->
            spawn(fun() -> 
                Result = apply(Fun, Chunk),
                self() ! {self(), Result}
            end)
        end,
        Chunks
    ).

% Join function to collect results from parallel tasks
join(Pids) ->
    lists:map(
        fun(Pid) ->
            receive
                {Pid, Result} -> Result
            end
        end,
        Pids
    ).

% Function to chunk the arguments for parallel processing
chunk_args(Args, NumChunks) ->
    ChunkSize = length(Args) div NumChunks,
    Remainder = length(Args) rem NumChunks,
    lists:map(
        fun(N) ->
            lists:sublist(Args, (N - 1) * ChunkSize + 1, 
                          ChunkSize + if N =< Remainder -> 1; true -> 0 end)
        end,
        lists:seq(1, NumChunks)
    ).
