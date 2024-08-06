-module(pipeline_parallel_pattern).
-export([run_pipeline/2]).

% Public API function
run_pipeline(Stages, InitialInput) ->
    % Start the pipeline with the initial input
    run_pipeline_stage(Stages, InitialInput, self()).

% Unified function to handle both base and recursive cases
run_pipeline_stage([], Input, ParentPid) ->
    % No more stages, send final result to the parent process
    ParentPid ! {pipeline_done, Input},
    receive
        {pipeline_done, Result} -> {ok, Result}
    end;

run_pipeline_stage([Stage | Rest], Input, ParentPid) ->
    % Start the stage process
    spawn(fun() -> 
        Result = Stage(Input),
        ParentPid ! {stage_done, Result}
    end),

    % Wait for this stage to complete and then process the result
    receive
        {stage_done, Result} ->
            run_pipeline_stage(Rest, Result, ParentPid)
    end.