-module(text_write_parallel).
-export([write_text/3]).

write_text(FilePath, TextList, N) ->
    %% Open file
    {ok, File} = file:open(FilePath, [write, raw]),
    %% Spawn processes to write text segments in parallel
    Pids = [spawn(fun() -> write_text_segment(File, Text, N) end) || Text <- TextList],
    %% Wait for all processes to finish
    lists:foreach(fun(Pid) -> receive {done, Pid} -> ok end end, Pids),
    %% Close file
    file:close(File).

write_text_segment(File, Text, N) ->
    lists:foreach(fun(_) -> io:format(File, "~s~n", [Text]) end, lists:seq(1, N)),
    self() ! {done, self()}.
