%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2017 redrock
%%% @doc list util.
%%% @end
%%%-------------------------------------------------------------------
-module(rock_rand).

-export([init/0, uniform/1]).

%%------------------------------------------------------------------------------
init() ->
    case get({?MODULE, had_seed}) of
        false ->
            rand:seed(exsplus, {erlang:monotonic_time(), erlang:time_offset(), erlang:unique_integer()}),
            put({?MODULE, had_seed}, true);
        _ ->
            skip
    end.

uniform(Int) ->
    init(),
    rand:uniform(Int).

