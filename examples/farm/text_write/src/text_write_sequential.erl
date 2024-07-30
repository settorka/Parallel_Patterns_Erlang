-module(text_write_sequential).
-export([write_text/3]).

write_text(FilePath, TextList, N) ->
    %% Open file
    {ok, File} = file:open(FilePath, [write, raw]),
    %% Write each text segment N times sequentially
    lists:foreach(fun(Text) -> write_text_n_times(File, Text, N) end, TextList),
    %% Close file
    file:close(File).

write_text_n_times(File, Text, N) ->
    lists:foreach(fun(_) -> io:format(File, "~s~n", [Text]) end, lists:seq(1, N)).
