-module(fork_join_sort).
-export([sort/1]).

-import(sequential_sort, [merge_sort/1, merge/2]).

% Function to sort an array using fork-join pattern
sort(Array) ->
    % Define the sorting function for each chunk
    Fun = fun([Chunk]) ->
                sequential_sort:merge_sort(Chunk)
          end,

    % Process the function using fork-join pattern
    SortedChunks = fork_join:process(Fun, chunk_array(Array)),

    % Merge the sorted chunks
    merge_sorted_chunks(SortedChunks).

% Function to chunk the array for parallel processing
chunk_array(Array) ->
    NumChunks = erlang:system_info(schedulers_online),
    ChunkSize = length(Array) div NumChunks,
    Remainder = length(Array) rem NumChunks,
    lists:map(fun(N) ->
                     lists:sublist(Array, (N - 1) * ChunkSize + 1,
                                   ChunkSize + if N =< Remainder -> 1; true -> 0 end)
              end, lists:seq(1, NumChunks)).

% Function to merge sorted chunks
merge_sorted_chunks([SingleChunk]) ->
    SingleChunk;
merge_sorted_chunks(Chunks) ->
    lists:foldl(fun(Chunk, Acc) ->
                        sequential_sort:merge(Acc, Chunk)
                end, [], Chunks).
