-module(pipeline_parallel_pattern).
-export([run_pipeline/2]).

%% Public API function
run_pipeline(Stages, InitialInput) ->
    % Start the pipeline with the initial input
    run_pipeline_stage(Stages, InitialInput, self(), #{}).

%% Unified function to handle both base and recursive cases
run_pipeline_stage([], Input, ParentPid, _) ->
    % No more stages, send final result to the parent process
    ParentPid ! {pipeline_done, Input},
    receive
        {pipeline_done, Result} -> {ok, Result}
    end;

run_pipeline_stage([Stage | Rest], Input, ParentPid, Acc) ->
    % Check if the result for the current stage is already cached
    case maps:find(Stage, Acc, undefined) of
        undefined ->
            % Stage result not cached, so process it
            spawn(fun() ->
                Result = Stage(Input),
                ParentPid ! {stage_done, Stage, Result}
            end),

            % Wait for this stage to complete and then process the result
            receive
                {stage_done, Stage, Result} ->
                    % Add result to cache and continue with the next stage
                    run_pipeline_stage(Rest, Result, ParentPid, maps:put(Stage, Result, Acc))
            end;
        CachedResult ->
            % Use cached result for the current stage
            run_pipeline_stage(Rest, CachedResult, ParentPid, Acc)
    end.
