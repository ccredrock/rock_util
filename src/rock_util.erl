%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2017 redrock
%%% @doc base util.
%%% @end
%%%-------------------------------------------------------------------
-module(rock_util).

-export([start/0]).

-export([init_ets/0,
         uniq_binary/0,
         to_binary/1,
         to_atom/1,
         to_list/1,
         human_string/1,
         human_binary/1]).

%%------------------------------------------------------------------------------
-spec start() -> {'ok', [atom()]} | {'error', term()}.
start() ->
    application:ensure_all_started(?MODULE).

%%------------------------------------------------------------------------------
-spec init_ets() -> ok.
init_ets() ->
    ets:new(?MODULE, [named_table, public]),
    ets:insert(?MODULE, {start_time, erlang:system_time()}).

-spec uniq_binary() -> binary().
uniq_binary() ->
    [{_, Time}] = ets:lookup(?MODULE, start_time),
    Start = integer_to_binary(Time, 32),
    Uniq = integer_to_binary(erlang:unique_integer([positive, monotonic]), 32),
    <<Start/binary, Uniq/binary>>.

%%------------------------------------------------------------------------------
-spec to_binary(binary() | list() | atom() | integer()) -> binary().
to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) when is_atom(X) -> atom_to_binary(X, utf8);
to_binary(X) when is_integer(X) -> integer_to_binary(X).

-spec to_list(list() | binary() | atom() | integer()) -> list().
to_list(X) when is_list(X) -> X;
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_integer(X) -> integer_to_list(X).

to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_binary(X) -> binary_to_atom(X, utf8);
to_atom(X) when is_integer(X) -> to_atom(integer_to_binary(X));
to_atom(X) -> to_atom(list_to_binary(human_string(X))).

human_string(X) ->
    case catch io_lib:format("~s", [X]) of
        {'EXIT', _} -> io_lib:format("~p", [X]);
        X1 -> X1
    end.

human_binary(X) ->
    list_to_binary(human_string(X)).

