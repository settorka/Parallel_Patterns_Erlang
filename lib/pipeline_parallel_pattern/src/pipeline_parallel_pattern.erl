-module(pipeline_parallel_pattern).
-export([run_pipeline/2]).

% Public API function
run_pipeline(Stages, InitialInput) ->
    % Determine if memoization should be used
    Memoize = should_use_memoization(Stages),
    
    % Start the pipeline with the initial input
    if
        Memoize ->
            % Use dynamic programming approach with caching
            run_pipeline_stage_memoized(Stages, InitialInput, self(), []);
        true ->
            % Use standard sequential approach
            run_pipeline_stage_standard(Stages, InitialInput, self())
    end.

% Determine if memoization should be used
should_use_memoization(Stages) ->
    % For simplicity, we check if there are repeated stages
    % In practice, you might want to implement a more complex check
    case lists:uniq(Stages) of
        UniqueStages when length(Stages) /= length(UniqueStages) -> true;
        _ -> false
    end.

% Standard pipeline processing without memoization
run_pipeline_stage_standard([], Input, ParentPid) ->
    % No more stages, send final result to the parent process
    ParentPid ! {pipeline_done, Input},
    receive
        {pipeline_done, Result} -> {ok, Result}
    end;

run_pipeline_stage_standard([Stage | Rest], Input, ParentPid) ->
    % Start the stage process
    spawn(fun() ->
        Result = Stage(Input),
        ParentPid ! {stage_done, Result}
    end),

    % Wait for this stage to complete and then process the result
    receive
        {stage_done, Result} ->
            run_pipeline_stage_standard(Rest, Result, ParentPid)
    end.

% Pipeline processing with memoization
run_pipeline_stage_memoized([], Input, ParentPid, _Acc) ->
    % No more stages, send final result to the parent process
    ParentPid ! {pipeline_done, Input},
    receive
        {pipeline_done, Result} -> {ok, Result}
    end;

run_pipeline_stage_memoized([Stage | Rest], Input, ParentPid, Acc) ->
    % Check if the result for the current stage is already cached
    case lists:keyfind(Stage, 1, Acc) of
        false ->
            % Start the stage process
            spawn(fun() ->
                Result = Stage(Input),
                ParentPid ! {stage_done, Stage, Result}
            end),

            % Wait for this stage to complete and then process the result
            receive
                {stage_done, Stage, Result} ->
                    % Add result to cache and continue with the next stage
                    run_pipeline_stage_memoized(Rest, Result, ParentPid, [{Stage, Result} | Acc])
            end;
        {Stage, CachedResult} ->
            % Use cached result for the current stage
            run_pipeline_stage_memoized(Rest, CachedResult, ParentPid, Acc)
    end.
