-module(pipeline_parallel_pattern).
-export([create_pipeline/1, run_pipeline/2]).

% Create a pipeline from a list of stage functions
create_pipeline(StageFunctions) ->
    Stages = [spawn_link(fun() -> stage_loop(Fun, self()) end) || Fun <- StageFunctions],
    connect_stages(Stages),
    Stages.

% Connect the stages in the pipeline
connect_stages([]) -> ok;
connect_stages([_LastStage]) -> ok;
connect_stages([Stage|Rest]) ->
    [NextStage|_] = Rest,
    Stage ! {set_next, NextStage},
    connect_stages(Rest).

% Stage loop
stage_loop(StageFunction, Parent) ->
    receive
        {set_next, NextStage} ->
            put(next_stage, NextStage),
            stage_loop(StageFunction, Parent);
        {data, Data} ->
            case get(next_stage) of
                undefined -> 
                    Parent ! {result, Data};
                NextStage ->
                    ProcessedData = parallel_stage(StageFunction, Data),
                    NextStage ! {data, ProcessedData}
            end,
            stage_loop(StageFunction, Parent)
    end.

% Parallel stage processing
parallel_stage(StageFunction, Data) ->
    NumCores = erlang:system_info(cores),
    Chunks = chunk_data(Data, NumCores),
    
    % Spawn worker processes to handle each chunk
    Pids = [spawn(fun() -> parallel_stage_loop(StageFunction, Chunk) end) || Chunk <- Chunks],
    
    % Collect results from worker processes
    Results = collect_results(Pids),
    
    % Flatten and return results
    lists:flatten(Results).

% Worker process loop for parallel stage processing
parallel_stage_loop(StageFunction, DataChunk) ->
    ProcessedData = StageFunction(DataChunk),
    self() ! {result, ProcessedData},
    receive
        stop -> ok
    end.

% Chunk data into smaller pieces
chunk_data(Data, NumChunks) ->
    ChunkSize = max(1, length(Data) div NumChunks),
    chunk_data(Data, ChunkSize, []).

chunk_data([], _, Acc) ->
    lists:reverse(Acc);
chunk_data(Data, ChunkSize, Acc) ->
    case lists:split(ChunkSize, Data) of
        {Chunk, Rest} ->
            chunk_data(Rest, ChunkSize, [Chunk | Acc])
    end.

% Collect results from worker processes using process monitoring
collect_results(Pids) ->
    %% Monitor all PIDs and wait for them to exit
    lists:map(fun(Pid) ->
        monitor(process, Pid),
        receive
            {'DOWN', _Ref, process, Pid, _Reason} ->
                {error, worker_failed};
            {Pid, Result} ->
                Result
        end
    end, Pids).

% Run the pipeline
run_pipeline(Stages, InitialData) ->
    StagesPids = create_pipeline(Stages),
    [FirstStage|_] = StagesPids,
    FirstStage ! {data, InitialData},
    receive
        {result, FinalResult} -> FinalResult
    after 60000 ->
        {error, timeout}
    end.
