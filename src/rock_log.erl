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
         hash_log_async/3,
         log_callback/1,
         log_callback/4,
         nt_alarm/4,
         nt_alarm_async/4]).

%%------------------------------------------------------------------------------
-define(HASH_LOG_NAME(N, H),
        binary_to_atom(<<"rock_log_", (rock_util:to_binary(N))/binary, "@", (integer_to_binary(H))/binary>>, utf8)).

%%------------------------------------------------------------------------------
lager_trace(Tags, Name, Size, Count) ->
    lager_trace(much_lager_event, Tags, Name, Size, Count).

lager_trace(Sink, Tags, Name, Size, Count) ->
    Server  = application:get_env(lager, servername, "dev"),
    AppKey  = application:get_env(lager, appkey, "dev"),
    LogName = application:get_env(lager, loggername, "dev"),
    lager:trace_file("much/" ++ atom_to_list(Tags) ++ "_" ++ atom_to_list(Name) ++ ".log",
                     [{Tags, Name}, {sink, Sink}],
                     info,
                     [{size, Size * 1024 * 1024},
                      {count, Count},
                      {formatter_config, [date, " ", time, " ",
                                          Server, " ", AppKey, " [info] ",
                                          atom_to_list(Name), " ", LogName, " ", message, "\n"]}]).
log_callback(Fun) ->
    ets:insert(rock_util, [{log_callback, Fun}]).

log_callback(Metric, Tags, Level, Msg) ->
    case ets:lookup(rock_util, log_callback) of
        [{_, Fun}] -> Fun(Metric, Tags, Level, Msg);
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

hash_log_async(Key, Name, Val) ->
    Hash = erlang:phash2(Key, 5),
    case erlang:whereis(ProcName = ?HASH_LOG_NAME(Name, Hash)) of
        undefined ->
         PID = spawn(fun Fun() ->
                        receive {log_msg, H, N, V} -> log_msg(H, N, V), erlang:garbage_collect(), Fun();
                                _ -> skip, Fun()
                        end
                     end),
         case catch erlang:register(ProcName, PID) of
             {'EXIT', _} -> whereis(ProcName) ! {log_msg, Hash, Name, Val};
             _ -> PID ! {log_msg, Hash, Name, Val}
         end;
      PID ->
         PID ! {log_msg, Hash, Name, Val}
    end.

log_msg(0, Name, #{xmdt := Tags, message := Val}) -> much0:info([{much0, Name}], "#XMDJ#~ts#XMDJ# ~p", [jsx:encode(Tags), Val]);
log_msg(1, Name, #{xmdt := Tags, message := Val}) -> much1:info([{much1, Name}], "#XMDJ#~ts#XMDJ# ~p", [jsx:encode(Tags), Val]);
log_msg(2, Name, #{xmdt := Tags, message := Val}) -> much2:info([{much2, Name}], "#XMDJ#~ts#XMDJ# ~p", [jsx:encode(Tags), Val]);
log_msg(3, Name, #{xmdt := Tags, message := Val}) -> much3:info([{much3, Name}], "#XMDJ#~ts#XMDJ# ~p", [jsx:encode(Tags), Val]);
log_msg(4, Name, #{xmdt := Tags, message := Val}) -> much4:info([{much4, Name}], "#XMDJ#~ts#XMDJ# ~p", [jsx:encode(Tags), Val]);
log_msg(0, Name, Val) -> much0:info([{much0, Name}], "~p", [Val]);
log_msg(1, Name, Val) -> much1:info([{much1, Name}], "~p", [Val]);
log_msg(2, Name, Val) -> much2:info([{much2, Name}], "~p", [Val]);
log_msg(3, Name, Val) -> much3:info([{much3, Name}], "~p", [Val]);
log_msg(4, Name, Val) -> much4:info([{much4, Name}], "~p", [Val]).

%%------------------------------------------------------------------------------
-spec nt_alarm(Metric::any(), Tags::any(), Level::atom(), Msg::any()) -> any().
nt_alarm(Metric, Tags, Level, Data) ->
    {Project, _} = init:script_id(),
    {ok, Url} = application:get_env(list_to_atom(Project), nt_url),
    {ok, Host} = inet:gethostname(),
    Body = jsx:encode([{<<"hostName">>, list_to_binary(Host)},
                       {<<"serverName">>, list_to_binary(Project)},
                       {<<"level">>, case Level of info -> 4; warning -> 3; error -> 1 end},
                       {<<"reason">>, rock_util:human_binary(map_get(reason, Data, {Metric, Tags}))},
                       {<<"msg">>, rock_util:human_binary(map_get(content, Data, Data))},
                       {<<"targetUrl">>, map_get(target, Data, <<>>)},
                       {<<"tenantId">>, map_get(tenant, Data, <<>>)}]),
    catch httpc:request(post, {Url, [], "application/json", Body}, [], []).

map_get(K, M, D) ->
    case catch maps:get(K, M) of
        {'EXIT', _} -> D;
        R -> R
    end.

-spec nt_alarm_async(Metric::any(), Tags::any(), Level::atom(), Msg::any()) -> any().
nt_alarm_async(Metric, Tags, Level, Data) ->
    spawn(fun() -> nt_alarm(Metric, Tags, Level, Data) end).

