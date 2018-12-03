%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2017 redrock
%%% @doc base gen.
%%% @end
%%%-------------------------------------------------------------------
-module(rock_gen).

-export([safe_send/3]).

-export([start/3, start/4, start_link/3, start_link/4, init/1,
         handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------
%% define
%%------------------------------------------------------------------------------
-record(state, {mod, cst, parasitic, flush, safe_seq}).

%%------------------------------------------------------------------------------
%% interface
%%------------------------------------------------------------------------------
safe_send(Fun, Msg, Cnt) ->
    Seq = erlang:unique_integer([monotonic]),
    case catch Fun() of
        {ok, PID} -> ensure_send(PID, {{?MODULE, safe_send}, Seq, Msg});
        _ when Cnt - 1 > 0 -> safe_send(Fun, Msg, Cnt - 1);
        Reason -> {error, Reason}
    end.

ensure_send(PID, Msg) ->
    Ref = erlang:monitor(process, PID),
    case erlang:is_process_alive(PID) of
        false -> {error, had_dead};
        true ->
            erlang:send(PID, Msg),
            case receive {'DOWN', Ref, process, _, _} -> false after 0 -> true end of
                false -> {error, be_dead};
                true -> erlang:demonitor(Ref), {ok, Msg}
            end
    end.

%%------------------------------------------------------------------------------
%% gen_server
%%------------------------------------------------------------------------------
start(Mod, Args, Options) ->
    gen_server:start(?MODULE, [Mod, Options, Args], Options).

start(Name, Mod, Args, Options) ->
    gen_server:start(Name, ?MODULE, [Mod, Options, Args], Options).

start_link(Mod, Args, Options) ->
    gen_server:start_link(?MODULE, [Mod, Options, Args], Options).

start_link(Name, Mod, Args, Options) ->
    gen_server:start_link(Name, ?MODULE, [Mod, Options, Args], Options).

init([Mod, Options,  Args]) ->
    case proplists:get_value(parasitic, Options) of
        undefined ->
            handle_init(Mod, Args, #state{mod = Mod});
        PID ->
            Ref = erlang:monitor(process, PID),
            case erlang:is_process_alive(PID) of
                false -> {stop, depend_dead};
                true -> handle_init(Mod, Args, #state{mod = Mod, parasitic = Ref})
            end
    end.

handle_call(Msg, From, #state{mod = Mod, cst = CS} = PS) ->
    case catch Mod:handle_call(Msg, From, CS) of
        {'EXIT', Reason} ->
            error_logger:warning_msg("rock_gen handle_call ~p", [Reason]),
            {reply, ok, PS#state{cst = CS}};
        Result ->
            format_result(Result, PS)
    end.

handle_cast(Msg, #state{mod = Mod, cst = CS} = PS) ->
    case catch Mod:handle_cast(Msg, CS) of
        {'EXIT', Reason} ->
            error_logger:warning_msg("rock_gen handle_cast ~p", [Reason]),
            {noreply, PS};
        Result ->
            format_result(Result, PS)
    end.

handle_info({{?MODULE, safe_send}, Seq, Msg}, #state{safe_seq = OldSeq} = PS) ->
    case OldSeq =/= undefined andalso Seq =< OldSeq of
        false ->
            handle_info(Msg, PS#state{safe_seq = Seq});
        true ->
            error_logger:warning_msg("rock_gen safe_send drop ~p", [{Seq, OldSeq, Msg}]),
            {noreply, PS}
    end;
handle_info({'DOWN', Ref, process, _, _}, #state{parasitic = Ref} = State) ->
    {stop, parasitic_dead, State};
handle_info(Msg, #state{mod = Mod, cst = CS} = PS) ->
    case catch Mod:handle_info(Msg, CS) of
        {'EXIT', Reason} ->
            error_logger:warning_msg("rock_gen handle_info ~p", [Reason]),
            {noreply, PS};
        Result ->
            format_result(Result, PS)
    end.

terminate(Reason, #state{mod = Mod, cst = CS} = _PS) ->
    Mod:terminate(Reason, CS).

code_change(OldVsn, #state{mod = Mod, cst = CS} = PS, Extra) ->
    case catch Mod:code_change(OldVsn, CS, Extra) of
        {'EXIT', _Reason} -> {ok, PS};
        Result -> format_result(Result, PS)
    end.

%%------------------------------------------------------------------------------
handle_init(Mod, Args, State) ->
    case catch Mod:init(Args) of
        {'EXIT', Reason} -> {stop, Reason};
        Result -> format_result(Result, State)
    end.

format_result(Result, PS) ->
    case Result of
        {ok, CS}                -> {ok, PS#state{cst = CS}};
        {ok, CS, T}             -> {ok, PS#state{cst = CS}, T};
        {reply, Result, CS}     -> {reply, Result, PS#state{cst = CS}};
        {reply, Result, CS, T}  -> {reply, Result, PS#state{cst = CS}, T};
        {noreply, CS}           -> {noreply, PS#state{cst = CS}};
        {noreply, CS, T}        -> {noreply, PS#state{cst = CS}, T};
        {stop, Reason, Rpl, CS} -> {stop, Reason, Rpl, PS#state{cst = CS}};
        {stop, Reason, CS}      -> {stop, Reason, PS#state{cst = CS}};
        _ -> Result
    end.

