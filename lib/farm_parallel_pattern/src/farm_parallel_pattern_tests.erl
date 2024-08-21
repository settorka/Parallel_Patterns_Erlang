-module(farm_parallel_pattern_tests).
-include_lib("eunit/include/eunit.hrl").

% Test for create_or_overwrite_file/1
% This test checks whether the create_or_overwrite_file/1 function correctly creates or overwrites a file.
% Parameters:
% - Filename: The name of the file to be created or overwritten.
% 
% Test Steps:
% 1. Create or overwrite a file named "test_file.txt".
% 2. Assert that the function returns 'ok', indicating success.
% 3. Assert that the file now exists using filelib:is_file/1.
% 4. Delete the file to clean up after the test.
create_or_overwrite_file_test() ->
    Filename = "test_file.txt",
    ?assertEqual(ok, farm_parallel_pattern:create_or_overwrite_file(Filename)),
    ?assert(filelib:is_file(Filename)),
    file:delete(Filename).

% Test for farm_work/4 with a simple worker function
% This test verifies that farm_work/4 correctly distributes a workload across multiple workers.
% Parameters:
% - Filename: The name of the file to which workers will append their output.
% - Workload: The total amount of work to distribute (100 units).
% - NumWorkers: The number of worker processes to be spawned (4 workers).
% - WorkerFun: A simple function where each worker writes the number of units it processed to a file.
%
% Test Steps:
% 1. Define a workload of 100 units and 4 workers.
% 2. Each worker writes its workload to "farm_work_test.txt".
% 3. Assert that the farm_work/4 function returns 'ok' after processing.
% 4. Read the file and split its contents into lines.
% 5. Assert that the file contains 4 lines, each corresponding to a worker's output.
% 6. Delete the file to clean up after the test.
farm_work_test() ->
    Filename = "farm_work_test.txt",
    Workload = 100,
    NumWorkers = 4,
    WorkerFun = fun(File, Units) ->
        {ok, IoDevice} = file:open(File, [append]),
        io:format(IoDevice, "Worker processed ~p units~n", [Units]),
        file:close(IoDevice)
    end,
    
    ?assertEqual(ok, farm_parallel_pattern:farm_work(Workload, NumWorkers, WorkerFun, Filename)),
    
    % Check if the file exists and contains the expected number of lines
    {ok, Contents} = file:read_file(Filename),
    Lines = string:split(Contents, "\n", all),
    ?assertEqual(NumWorkers, length(Lines) - 1),  % -1 for the last empty line
    
    file:delete(Filename).

% Test for error handling in create_or_overwrite_file/1
% This test checks how create_or_overwrite_file/1 handles errors when trying to create a file in a non-existent directory.
% Parameters:
% - Filename: The name of the file in a non-existent directory.
%
% Test Steps:
% 1. Attempt to create a file in a non-existent directory.
% 2. Assert that the function returns an error tuple {error, Reason}.
create_or_overwrite_file_error_test() ->
    Filename = "/nonexistent_directory/test_file.txt",
    ?assertMatch({error, _}, farm_parallel_pattern:create_or_overwrite_file(Filename)).

% Test for farm_work/4 with an error in file creation
% This test verifies that farm_work/4 properly handles errors when the output file cannot be created.
% Parameters:
% - Filename: The name of the file in a non-existent directory.
% - Workload: The total amount of work to distribute (100 units).
% - NumWorkers: The number of worker processes to be spawned (4 workers).
% - WorkerFun: A simple function that does nothing in this test case.
%
% Test Steps:
% 1. Define a workload and worker configuration.
% 2. Attempt to run farm_work/4 with a file in a non-existent directory.
% 3. Assert that the function returns an error tuple {error, Reason}.
farm_work_file_error_test() ->
    Filename = "/nonexistent_directory/farm_work_test.txt",
    Workload = 100,
    NumWorkers = 4,
    WorkerFun = fun(_, _) -> ok end,
    
    ?assertMatch({error, _}, farm_parallel_pattern:farm_work(Workload, NumWorkers, WorkerFun, Filename)).
