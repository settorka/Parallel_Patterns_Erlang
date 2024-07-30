-module(text_write_farm).
-export([write_text/3]).

write_text(FilePath, TextList, N) ->
    %% Number of workers
    NumWorkers = 4,
    %% Function to write a single text segment N times
    Fun = fun(Text) -> write_text_segment(FilePath, Text, N) end,
    %% Use the farm parallel pattern
    farm_parallel_pattern:start(NumWorkers, Fun, TextList),
    %% No need to stop workers manually here.
    ok.

write_text_segment(FilePath, Text, N) ->
    {ok, File} = file:open(FilePath, [append, raw]),
    lists:foreach(fun(_) -> io:format(File, "~s~n", [Text]) end, lists:seq(1, N)),
    file:close(File).
