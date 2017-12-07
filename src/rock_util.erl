%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2017 redrock
%%% @doc base util.
%%% @end
%%%-------------------------------------------------------------------
-module(rock_util).

-export([start/0, start_time/0, global_uniq/0]).

%%------------------------------------------------------------------------------
-spec start() -> {'ok', [atom()]} | {'error', term()}.
start() ->
    application:ensure_all_started(?MODULE).

-spec start_time() -> pos_integer().
start_time() ->
    case application:get_env(?MODULE, start_time) of
        undefined -> application:set_env(?MODULE, start_time, Time = erlang:system_time()), Time;
        {ok, Time} -> Time
    end.

-spec global_uniq() -> binary().
global_uniq() ->
    Start = integer_to_binary(start_time(), 32),
    Uniq = integer_to_binary(erlang:unique_integer([positive, monotonic]), 32),
    <<Start/binary, Uniq/binary>>.

