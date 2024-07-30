-module(factorial_farm).
-export([compute_factorials/2]).

compute_factorials(NumList, NumWorkers) ->
    %% Function to compute the factorial of a single number
    Fun = fun(Num) -> compute_factorial(Num) end,
    %% Use the farm parallel pattern
    farm_parallel_pattern:start(NumWorkers, Fun, NumList),
    %% No need to stop workers manually here.
    ok.

compute_factorial(Num) ->
    Factorial = factorial(Num),
    io:format("Factorial of ~p is ~p~n", [Num, Factorial]).

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).
