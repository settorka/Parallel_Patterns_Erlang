-module(log_analysis_sequential).
-export([run/1, analyze_log/1]).

% Entry point to start the log analysis
run(Filename) ->
    analyze_log(Filename).

analyze_log(Filename) ->
    % Stage 1: Read the file
    Data = read_file(Filename),
    
    % Stage 2: Parse the log data
    ParsedLogs = parse_logs(Data),
    
    % Stage 3: Filter the parsed logs
    FilteredLogs = filter_logs(ParsedLogs),
    
    % Stage 4: Aggregate the filtered logs
    AggregatedData = aggregate_data(FilteredLogs),
    
    % Stage 5: Generate alerts based on the aggregated data
    AlertedData = generate_alerts(AggregatedData),
    
    % Stage 6: Format the results for output
    FormattedData = format_output(AlertedData),
    
    % Stage 7: Write the formatted results to a file
    write_output(FormattedData).

% Function to read a file
read_file(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    lists:sublist(Lines, 2, length(Lines) - 1). % Skip header line

% Function to parse log entries into structured data
parse_logs(Lines) ->
    [parse_line(Line) || Line <- Lines].

parse_line(Line) ->
    [Date, Temp, Humidity, Light, Time, RecordedBy] = binary:split(Line, <<",">>, [global]),
    #{date => Date,
      temp => binary_to_integer(Temp),
      humidity => binary_to_integer(Humidity),
      light_intensity => binary_to_integer(Light),
      time => Time,
      recorded_by => RecordedBy}.

% Function to filter logs based on criteria
filter_logs(Logs) ->
    TempThreshold = 30, % Example threshold
    HumidityThreshold = 70, % Example threshold
    [Log || Log = #{temp := Temp, humidity := Humidity} <- Logs, 
            Temp >= TempThreshold, Humidity >= HumidityThreshold].

% Function to aggregate data from logs
aggregate_data(Logs) ->
    TempCount = length([L || #{temp := Temp} = L <- Logs, Temp >= 30]),
    HumidityCount = length([L || #{humidity := Humidity} = L <- Logs, Humidity >= 70]),
    #{temp_count => TempCount, humidity_count => HumidityCount, logs => Logs}.

% Function to generate alerts based on aggregated data
generate_alerts(#{temp_count := TempCount, humidity_count := HumidityCount} = Data) when TempCount > 10 orelse HumidityCount > 10 ->
    Data#{alerts => [high_temp_or_humidity]};
generate_alerts(Data) ->
    Data#{alerts => []}.

% Function to format data into a JSON structure
format_output(#{temp_count := TempCount, humidity_count := HumidityCount, logs := Logs, alerts := Alerts}) ->
    jiffy:encode(#{
        summary => #{
            temp_count => TempCount,
            humidity_count => HumidityCount
        },
        alerts => Alerts,
        logs => Logs
    }).

% Function to write formatted data to a file
write_output(FormattedData) ->
    file:write_file("output.json", FormattedData),
    {ok, "output.json"}.
