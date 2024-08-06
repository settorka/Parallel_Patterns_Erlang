-module(log_analysis_parallel).
-export([run/0, analyze_log/1, stage_1/2, stage_2/2, stage_3/2, stage_4/2, stage_5/2, stage_6/2, stage_7/2]).

% Entry point to start the log analysis
run() ->
    Filename = "log_file.txt",
    analyze_log(Filename).

analyze_log(Filename) ->
    self() ! {start, Filename},
    loop().

% Loop to receive messages and handle each stage
loop() ->
    receive
        {start, Filename} ->
            Pid1 = spawn(?MODULE, stage_1, [self(), Filename]),
            monitor(process, Pid1),
            loop();
        {stage_1_done, Data} ->
            Pid2 = spawn(?MODULE, stage_2, [self(), Data]),
            monitor(process, Pid2),
            loop();
        {stage_2_done, ParsedLogs} ->
            Pid3 = spawn(?MODULE, stage_3, [self(), ParsedLogs]),
            monitor(process, Pid3),
            loop();
        {stage_3_done, FilteredLogs} ->
            Pid4 = spawn(?MODULE, stage_4, [self(), FilteredLogs]),
            monitor(process, Pid4),
            loop();
        {stage_4_done, AggregatedData} ->
            Pid5 = spawn(?MODULE, stage_5, [self(), AggregatedData]),
            monitor(process, Pid5),
            loop();
        {stage_5_done, AlertedData} ->
            Pid6 = spawn(?MODULE, stage_6, [self(), AlertedData]),
            monitor(process, Pid6),
            loop();
        {stage_6_done, FormattedData} ->
            Pid7 = spawn(?MODULE, stage_7, [self(), FormattedData]),
            monitor(process, Pid7),
            loop();
        {stage_7_done, Result} ->
            io:format("Analysis complete: ~p~n", [Result]);
        {'DOWN', _Ref, process, _Pid, _Reason} ->
            io:format("Process crashed: ~p~n", [_Reason]),
            loop()
    end.

% Stage 1: Read the file
stage_1(Parent, Filename) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    Parent ! {stage_1_done, lists:sublist(Lines, 2, length(Lines) - 1)}.

% Stage 2: Parse the log data
stage_2(Parent, Lines) ->
    ParsedLogs = [parse_line(Line) || Line <- Lines],
    Parent ! {stage_2_done, ParsedLogs}.

parse_line(Line) ->
    [Date, Temp, Humidity, Light, Time, RecordedBy] = binary:split(Line, <<",">>, [global]),
    #{date => Date,
      temp => binary_to_integer(Temp),
      humidity => binary_to_integer(Humidity),
      light_intensity => binary_to_integer(Light),
      time => Time,
      recorded_by => RecordedBy}.

% Stage 3: Filter the parsed logs
stage_3(Parent, Logs) ->
    TempThreshold = 30, % Example threshold
    HumidityThreshold = 70, % Example threshold
    FilteredLogs = [Log || Log = #{temp := Temp, humidity := Humidity} <- Logs, 
                           Temp >= TempThreshold, Humidity >= HumidityThreshold],
    Parent ! {stage_3_done, FilteredLogs}.

% Stage 4: Aggregate the filtered logs
stage_4(Parent, Logs) ->
    TempCount = length([L || #{temp := Temp} = L <- Logs, Temp >= 30]),
    HumidityCount = length([L || #{humidity := Humidity} = L <- Logs, Humidity >= 70]),
    AggregatedData = #{temp_count => TempCount, humidity_count => HumidityCount, logs => Logs},
    Parent ! {stage_4_done, AggregatedData}.

% Stage 5: Generate alerts based on aggregated data
stage_5(Parent, #{temp_count := TempCount, humidity_count := HumidityCount} = Data) when TempCount > 10 orelse HumidityCount > 10 ->
    AlertedData = Data#{alerts => [high_temp_or_humidity]},
    Parent ! {stage_5_done, AlertedData};
stage_5(Parent, Data) ->
    AlertedData = Data#{alerts => []},
    Parent ! {stage_5_done, AlertedData}.

% Stage 6: Format the results for output
stage_6(Parent, #{temp_count := TempCount, humidity_count := HumidityCount, logs := Logs, alerts := Alerts}) ->
    FormattedData = jiffy:encode(#{
        summary => #{
            temp_count => TempCount,
            humidity_count => HumidityCount
        },
        alerts => Alerts,
        logs => Logs
    }),
    Parent ! {stage_6_done, FormattedData}.

% Stage 7: Write the formatted results to a file
stage_7(Parent, FormattedData) ->
    file:write_file("output.json", FormattedData),
    Parent ! {stage_7_done, {ok, "output.json"}}.
