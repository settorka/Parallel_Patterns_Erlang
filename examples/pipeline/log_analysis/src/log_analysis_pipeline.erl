-module(log_analysis_pipeline).
-export([run/0]).

run() ->
    % Define the filename for the log analysis
    Filename = "log_file.txt",  % Change this as needed
    
    % Define the stages for the pipeline
    Stages = [
        fun log_analysis_sequential:read_file/1,
        fun log_analysis_sequential:parse_logs/1,
        fun log_analysis_sequential:filter_logs/1,
        fun log_analysis_sequential:aggregate_data/1,
        fun log_analysis_sequential:generate_alerts/1,
        fun log_analysis_sequential:format_output/1,
        fun log_analysis_sequential:write_output/1
    ],
    
    % Run the pipeline with the defined stages
    Result = pipeline_parallel_pattern:run_pipeline(Stages, Filename),
    
    ResultFileName = "o",
    % Handles the result
    case Result of
        {ok, ResultFileName} ->
            io:format("Log analysis complete. Output written to ~s~n", [ResultFileName]);
        {error, Reason} ->
            io:format("Log analysis failed: ~p~n", [Reason])
    end.

