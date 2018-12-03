%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2018 redrock
%%% @doc rock log
%%% @end
%%%-------------------------------------------------------------------
-ifndef(ROCK_LOG2_HRL).
-define(ROCK_LOG2_HRL, true).

-define(LOG(Name, X),
        much:info([{much, Name}], "~p", [X])).

-define(LOG(Name, Tags, X),
        much:info([{much, Name}], "#XMDJ#~ts#XMDJ# ~p", [Tags, X])).

%%------------------------------------------------------------------------------
-define(LOG_INC(Metric, Tags, Data),
        begin
            flow_falcon2:add_count(Metric, Tags, 1),
            ?LOG(Metric, {Tags, Data})
        end).

-define(LOG_TAGS(Metric, Tags, Index, Data),
        begin
            flow_falcon2:add_count(Metric, Tags, 1),
            ?LOG(Metric, jsx:encode((Index)#{metric_ => Metric, tags_ => rock_util:human_binary(Tags)}), Data)
        end).

-define(LOG_SET(Metric, Val, Data),
        begin
            flow_falcon2:set_count(Metric, Val),
            ?LOG(Metric, {Val, Data})
        end).

-define(LOG_SET(Metric, Tags, Val, Data),
        begin
            flow_falcon2:set_count(Metric, Tags, Val),
            ?LOG(Metric, {Val, Tags, Data})
        end).

%%------------------------------------------------------------------------------
-define(LOG_DEBUG(Metric, Tags, Msg),
        begin
            flow_falcon2:add_count(Metric, Tags, 1),
            lager:debug("~p:~p ~p", [Metric, Tags, Msg])
        end).

-define(LOG_INFO(Metric, Tags, Msg),
        begin
            flow_falcon2:add_count(Metric, Tags, 1),
            lager:info("~p:~p ~p", [Metric, Tags, Msg]),
            rock_log:log_callback(Metric, Tags, info, Reason)
        end).

-define(LOG_WARNING(Metric, Tags, Reason),
        begin
            flow_falcon2:add_count(Metric, Tags, 1),
            lager:warning("~p:~p ~p", [Metric, Tags, Reason]),
            rock_log:log_callback(Metric, Tags, warning, Reason)
        end).

-define(LOG_ERROR(Metric, Tags, Reason),
        begin
            flow_falcon2:add_count(Metric, Tags, 1),
            lager:error("~p:~p ~p", [Metric, Tags, Reason]),
            rock_log:log_callback(Metric, Tags, error, Reason)
        end).

-define(LOG_STOP(R),
        begin
            R =/= normal andalso R =/= shutdown andalso ?LOG_ERROR(process_stop, ?MODULE, R),
            ok
        end).

-endif.

