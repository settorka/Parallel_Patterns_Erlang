-module(sequential_sort).
-export([sort/1]).

% Sequential sorting using merge sort
sort(Array) ->
    merge_sort(Array).

merge_sort([]) ->
    [];
merge_sort([X]) ->
    [X];
merge_sort(List) ->
    {L1, L2} = lists:split(length(List) div 2, List),
    merge(merge_sort(L1), merge_sort(L2)).

merge([], L) ->
    L;
merge(L, []) ->
    L;
merge([H1|T1] = L1, [H2|T2] = L2) ->
    if
        H1 =< H2 ->
            [H1|merge(T1, L2)];
        true ->
            [H2|merge(L1, T2)]
    end.