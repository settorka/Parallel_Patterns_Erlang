-module(pipeline_parallel_pattern).
-export([run_pipeline/2]).

%% Public API function
% run_pipeline/2 initializes the pipeline processing with the given stages and initial input.
% Parameters:
% - Stages: A list of functions (stages) to be applied sequentially.
% - InitialInput: The initial input data to be processed by the first stage.
%
% Expected Output:
% - Returns {ok, Result}, where Result is the output after all stages have processed the input.
% - Handles pipeline errors gracefully by reporting the failure.
run_pipeline(Stages, InitialInput) ->
    %% Start the pipeline with the initial input
    run_pipeline_stage(Stages, InitialInput, self()).

%% Base case: no more stages
% This clause handles the scenario when there are no more stages left in the pipeline.
% Parameters:
% - []: An empty list indicating that all stages have been processed.
% - Input: The final result after all previous stages have processed it.
% - ParentPid: The process ID of the parent (initial caller) that started the pipeline.
%
% Expected Output:
% - Sends the final result to the parent process and returns {ok, Result}.
run_pipeline_stage([], Input, ParentPid) ->
    %% No more stages, send final result to the parent process
    ParentPid ! {pipeline_done, Input},
    receive
        {pipeline_done, Result} -> {ok, Result}
    end;

%% Recursive case: apply the current stage and proceed to the next
% This clause processes each stage in the pipeline recursively.
% Parameters:
% - [Stage | Rest]: A list where Stage is the current function to apply, and Rest is the remaining stages.
% - Input: The current input to be processed by the Stage function.
% - ParentPid: The process ID of the parent (initial caller) that started the pipeline.
%
% Expected Output:
% - Applies the Stage function to the Input, then proceeds to process the remaining stages.
% - Handles process failures by sending an appropriate message to the parent process.
run_pipeline_stage([Stage | Rest], Input, ParentPid) ->
    %% Start the stage process and monitor it
    Pid = spawn(fun() ->
        Result = Stage(Input),
        ParentPid ! {stage_done, Result}
    end),
    Ref = erlang:monitor(process, Pid),

    %% Wait for this stage to complete, either normally or with an error
    receive
        {'DOWN', Ref, process, _Pid, Reason} ->
            %% Handle process termination
            ParentPid ! {stage_failed, Reason};
        {stage_done, Result} ->
            %% Recursively process the rest of the stages
            erlang:demonitor(Ref, [flush]),
            run_pipeline_stage(Rest, Result, ParentPid)
    end.
