%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2018 redrock
%%% @doc rock log
%%% @end
%%%-------------------------------------------------------------------
-ifndef(ROCK_LOG_HRL).
-define(ROCK_LOG_HRL, true).

-define(LOG(Name, X),
        much:info([{much, Name}], "~p", [X])).

-define(LOG(Name, Tags, X),
        much:info([{much, Name}], "#XMDJ#~ts#XMDJ# ~p", [Tags, X])).

-define(LOG_INC(OP, Data),
        begin
            flow_falcon:inc_total(OP),
            ?LOG(OP, Data)
        end).

-define(LOG_INC(OP, Type, Data),
        begin
            flow_falcon:inc_total(OP, Type),
            ?LOG(OP, {Type, Data})
        end).

-define(LOG_TAGS_INC(OP, Type, Tags, Data),
        begin
            flow_falcon:inc_total(OP, Type),
            ?LOG(OP, jsx:encode((Tags)#{logop => OP, logtype => Type}), Data)
        end).

-define(LOG_SET(OP, Val, Data),
        begin
            flow_falcon:set_total(OP, Val),
            ?LOG(OP, {Val, Data})
        end).

-define(LOG_SET(OP, Type, Val, Data),
        begin
            flow_falcon:set_total(OP, Type, Val),
            ?LOG(OP, {Val, Type, Data})
        end).

-define(LOG_DEBUG(OP, Type, Msg),
        begin
            flow_falcon:inc_total(OP, Type),
            lager:debug("~p:~p ~p", [OP, Type, Msg])
        end).

-define(LOG_INFO(OP, Type, Msg),
        begin
            flow_falcon:inc_total(OP, Type),
            lager:info("~p:~p ~p", [OP, Type, Msg]),
            rock_log:log_callback(OP, Type, info, Reason)
        end).

-define(LOG_WARNING(OP, Type, Reason),
        begin
            flow_falcon:inc_total(OP, Type),
            lager:warning("~p:~p ~p", [OP, Type, Reason]),
            rock_log:log_callback(OP, Type, warning, Reason)
        end).

-define(LOG_ERROR(OP, Type, Reason),
        begin
            flow_falcon:inc_total(OP, Type),
            lager:error("~p:~p ~p", [OP, Type, Reason]),
            rock_log:log_callback(OP, Type, error, Reason)
        end).

-define(LOG_STOP(R),
        begin
            R =/= normal andalso R =/= shutdown andalso ?LOG_ERROR(process_stop, ?MODULE, R),
            ok
        end).

-endif.

