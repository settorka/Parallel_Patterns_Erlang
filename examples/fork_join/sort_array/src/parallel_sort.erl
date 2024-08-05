-module(parallel_sort).
-export([sort/1]).

% Parallel sorting using merge sort
sort(Array) ->
    % Get the number of schedulers available
    NumSchedulers = erlang:system_info(schedulers_online),

    % Divide the array into chunks
    Chunks = chunk_array(Array, NumSchedulers),

    % Perform merge sort on each chunk in parallel
    Pids = lists:map(fun(Chunk) ->
                             spawn(fun() -> merge_sort(Chunk, self()) end)
                     end, Chunks),

    % Collect sorted chunks from all processes
    SortedChunks = lists:map(fun(Pid) -> receive {Pid, SortedChunk} -> SortedChunk end end, Pids),
    
    % Merge sorted chunks into the final sorted array
    FinalSortedArray = parallel_merge(SortedChunks),

    % Return the resulting sorted array
    FinalSortedArray.

% Function to divide the array into chunks
chunk_array(Array, NumChunks) ->
    {ChunkSize, Remainder} = {length(Array) div NumChunks, length(Array) rem NumChunks},
    lists:map(fun(N) ->
                     lists:sublist(Array, (N - 1) * ChunkSize + 1, 
                     ChunkSize + if N =< Remainder -> 1; true -> 0 end)
              end, lists:seq(1, NumChunks)).

% Function to perform merge sort and send the result back to the parent process
merge_sort(List, ParentPid) ->
    SortedList = sequential_merge_sort(List),
    ParentPid ! {self(), SortedList}.

% Sequential merge sort
sequential_merge_sort([]) ->
    [];
sequential_merge_sort([X]) ->
    [X];
sequential_merge_sort(List) ->
    {L1, L2} = lists:split(length(List) div 2, List),
    merge(sequential_merge_sort(L1), sequential_merge_sort(L2)).

% Function to merge two sorted lists
merge([], L) ->
    L;
merge(L, []) ->
    L;
merge([H1|T1] = L1, [H2|T2] = L2) ->
    if
        H1 =< H2 ->
            [H1|merge(T1, L2)];
        true ->
            [H2|merge(L1, T2)]
    end.

% Function to merge a list of sorted chunks into a single sorted list
parallel_merge([Chunk]) ->
    Chunk;
parallel_merge(Chunks) ->
    {Left, Right} = lists:split(length(Chunks) div 2, Chunks),
    merge(parallel_merge(Left), parallel_merge(Right)).
