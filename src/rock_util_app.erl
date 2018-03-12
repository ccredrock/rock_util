%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2017 redrock
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(rock_util_app).

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
-behaviour(application).

%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    rock_util:init_ets(),
    rock_util_sup:start_link().

stop(_State) ->
    ok.
