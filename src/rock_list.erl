%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2017 redrock
%%% @doc list util.
%%% @end
%%%-------------------------------------------------------------------
-module(rock_list).

-export([uniq_add/2,
         any_hit/2,
         any_res/2,
         rand_one/1,
         rand_all/1]).

%%------------------------------------------------------------------------------
-spec uniq_add(Val::any(), List::list()) -> list().
uniq_add(Val, List) ->
    case lists:member(Val, List) of
        true -> List;
        false -> [Val | List]
    end.

%%------------------------------------------------------------------------------
-spec any_hit(Fun::fun(), List::list()) -> list().
any_hit(_Fun, []) -> error;
any_hit(Fun, [H | T]) ->
    case Fun(H) of
        true -> {ok, H};
        false -> any_hit(Fun, T)
    end.

any_res(_Fun, []) -> error;
any_res(Fun, [H | T]) ->
    case catch Fun(H) of
        {'EXIT', _} -> any_res(Fun, T);
        Res -> {ok, Res}
    end.

%%------------------------------------------------------------------------------
rand_one([]) -> undefined;
rand_one(List) ->
    lists:nth(rock_rand:uniform(length(List)), List).

%%------------------------------------------------------------------------------
rand_all([]) -> [];
rand_all(List) ->
    Len = length(List),
    [X || {_, X} <- lists:sort([{rock_rand:uniform(Len), X} || X <- List])].

