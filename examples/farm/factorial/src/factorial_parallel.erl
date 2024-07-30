-module(factorial_parallel).
-export([compute_factorials/1]).

compute_factorials(NumList) ->
    %% Create a process for each factorial computation
    Pids = [spawn(fun() -> compute_factorial(Num) end) || Num <- NumList],
    %% Wait for all processes to finish
    lists:foreach(fun(Pid) -> receive_done(Pid) end, Pids).

compute_factorial(Num) ->
    Factorial = factorial(Num),
    io:format("Factorial of ~p is ~p~n", [Num, Factorial]),
    %% Signal completion
    self() ! done.

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

receive_done(_Pid) ->
    %% Wait for a signal from each process
    receive
        done -> ok
    end.
