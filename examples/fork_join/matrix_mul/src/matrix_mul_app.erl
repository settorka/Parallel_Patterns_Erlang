%%%-------------------------------------------------------------------
%% @doc matrix_mul public API
%% @end
%%%-------------------------------------------------------------------

-module(matrix_mul_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    matrix_mul_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
