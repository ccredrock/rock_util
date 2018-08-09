%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2017 redrock
%%% @doc base log.
%%% @end
%%%-------------------------------------------------------------------
-module(rock_log).

-export([lager_trace/4,
         much_trace/3,
         hash_log/3,
         log_callback/1,
         log_callback/4,
         nt_alarm/4]).

%%------------------------------------------------------------------------------
lager_trace(Type, Name, Size, Count) ->
    lager_trace(much_lager_event, Type, Name, Size, Count).

lager_trace(Sink, Type, Name, Size, Count) ->
    Server  = application:get_env(lager, servername, "dev"),
    AppKey  = application:get_env(lager, appkey, "dev"),
    LogName = application:get_env(lager, loggername, "dev"),
    lager:trace_file(atom_to_list(Type) ++ "/" ++ atom_to_list(Name) ++ ".log",
                     [{Type, Name}, {sink, Sink}],
                     info,
                     [{size, Size * 1024 * 1024},
                      {count, Count},
                      {formatter_config, [date, " ", time, " ",
                                          Server, " ", AppKey, " [info] ",
                                          atom_to_list(Name), " ", LogName, " ", message, "\n"]}]).
log_callback(Fun) ->
    ets:insert(rock_util, [{log_callback, Fun}]).

log_callback(OP, Type, Level, Msg) ->
    case ets:lookup(rock_util, log_callback) of
        [{_, Fun}] -> Fun(OP, Type, Level, Msg);
        [] -> skip
    end.

%%------------------------------------------------------------------------------
much_trace(Name, Size, Count) ->
    lager_trace(much0_lager_event, much0, Name, Size, Count),
    lager_trace(much1_lager_event, much1, Name, Size, Count),
    lager_trace(much2_lager_event, much2, Name, Size, Count),
    lager_trace(much3_lager_event, much3, Name, Size, Count),
    lager_trace(much4_lager_event, much4, Name, Size, Count).

hash_log(Key, Name, Val) ->
    log_msg(erlang:phash2(Key, 5), Name, Val).

log_msg(0, Name, Val) -> much0:info([{much0, Name}], "~p", [Val]);
log_msg(1, Name, Val) -> much1:info([{much1, Name}], "~p", [Val]);
log_msg(2, Name, Val) -> much2:info([{much2, Name}], "~p", [Val]);
log_msg(3, Name, Val) -> much3:info([{much3, Name}], "~p", [Val]);
log_msg(4, Name, Val) -> much4:info([{much4, Name}], "~p", [Val]).

%%------------------------------------------------------------------------------
-spec nt_alarm(OP::any(), Type::any(), Level::atom(), Msg::any()) -> any().
nt_alarm(OP, Type, Level, Data) ->
    {Project, _} = init:script_id(),
    {ok, Url} = application:get_env(list_to_atom(Project), nt_url),
    {ok, Host} = inet:gethostname(),
    Body = jsx:encode([{<<"hostName">>, list_to_binary(Host)},
                       {<<"serverName">>, list_to_binary(Project)},
                       {<<"level">>, case Level of info -> 4; warning -> 3; error -> 1 end},
                       {<<"reason">>, list_to_binary(rock_util:human(map_get(reason, Data, {OP, Type})))},
                       {<<"msg">>, list_to_binary(rock_util:human(map_get(content, Data, Data)))},
                       {<<"targetUrl">>, map_get(target, Data, <<>>)},
                       {<<"tenantId">>, map_get(tenant, Data, <<>>)}]),
    catch httpc:request(post, {Url, [], "application/json", Body}, [], []).

map_get(K, M, D) ->
    case catch maps:get(K, M) of
        {'EXIT', _} -> D;
        R -> R
    end.

