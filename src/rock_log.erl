%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2017 redrock
%%% @doc base log.
%%% @end
%%%-------------------------------------------------------------------
-module(rock_log).

-export([lager_trace/4,
         log_callback/1,
         log_callback/4]).

%%------------------------------------------------------------------------------
lager_trace(Type, Name, Size, Count) ->
    Server  = application:get_env(lager, servername, "dev"),
    AppKey  = application:get_env(lager, appkey, "dev"),
    LogName = application:get_env(lager, loggername, "dev"),
    lager:trace_file(atom_to_list(Type) ++ "/" ++ atom_to_list(Name) ++ ".log",
                     [{Type, Name},
                      {sink, much_lager_event}],
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

