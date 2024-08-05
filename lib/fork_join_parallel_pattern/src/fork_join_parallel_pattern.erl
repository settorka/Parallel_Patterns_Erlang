-module(fork_join_parallel_pattern).
-export([fork/2, join/1]).

% Fork function to divide the work
fork(Fun, Args) ->
    % Determine the number of schedulers available
    NumSchedulers = erlang:system_info(schedulers_online),
    
    % Chunk the arguments
    Chunks = chunk_args(Args, NumSchedulers),
    
    % Spawn processes for each chunk
    Pids = lists:map(fun(Chunk) ->
                             spawn(fun() -> 
                                        execute_chunk(Fun, Chunk, self())
                             end)
                     end, Chunks),
    
    % Return PIDs to join later
    Pids.

% Join function to collect and combine results
join(Pids) ->
    % Collect results from all processes
    Results = lists:map(fun(Pid) -> 
                             receive {Pid, Result} -> Result end
                      end, Pids),
    
    % Combine results
    combine_results(Results).

% Function to divide arguments into chunks
chunk_args(Args, NumChunks) ->
    {ChunkSize, Remainder} = {length(Args) div NumChunks, length(Args) rem NumChunks},
    lists:map(fun(N) ->
                     lists:sublist(Args, (N - 1) * ChunkSize + 1,
                                   ChunkSize + if N =< Remainder -> 1; true -> 0 end)
              end, lists:seq(1, NumChunks)).

% Function to execute the given function on each chunk
execute_chunk(Fun, Chunk, ParentPid) ->
    Result = apply(Fun, Chunk),
    ParentPid ! {self(), Result}.

% Function to combine results from all processes
combine_results(Results) ->
    lists:flatten(Results).  % Default combination function, override if needed.
