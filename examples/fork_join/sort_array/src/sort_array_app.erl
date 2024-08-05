%%%-------------------------------------------------------------------
%% @doc sort_array public API
%% @end
%%%-------------------------------------------------------------------

-module(sort_array_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sort_array_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
