-module(factorial_sequential).
-export([compute_factorials/1]).

compute_factorials(NumList) ->
    %% Compute factorials sequentially
    lists:map(fun(Num) -> compute_factorial(Num) end, NumList).

compute_factorial(Num) ->
    Factorial = factorial(Num),
    io:format("Factorial of ~p is ~p~n", [Num, Factorial]).

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).
