%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2017 redrock
%%% @doc list util.
%%% @end
%%%-------------------------------------------------------------------
-module(rock_list).

-export([uniq_add/2]).

%%------------------------------------------------------------------------------
-spec uniq_add(Val::any(), List::list()) -> list().
uniq_add(Val, List) ->
    case lists:member(Val, List) of
        true -> List;
        false -> [Val | List]
    end.

