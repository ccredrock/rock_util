%%%-------------------------------------------------------------------
%%% @author wanghongyan05
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-ifndef(ROCK_DEFAULT_HRL).
-define(ROCK_DEFAULT_HRL, true).

%%------------------------------------------------------------------------------
hl() ->
    [{"hs/1",  "host swap"},
     {"cr/1",  "to core"},
     {"ca/1",  "to asm"},
     {"ce/1",  "export all"},
     {"dm/1",  "decompile"},
     {"pj/0",  "project name"},
     {"vs/0",  "project version"},
     {"log/2", "log term to file"},
     {"pf/2",  "recon profile"},
     {"em/2",  "etop memory"},
     {"er/2",  "etop reductions"},
     {"eq/2",  "etop message queue"},
     {"es/2",  "etop stop"},
     {"fp/1",  "fprof"},
     {"ep/1",  "eprof"}].

%%------------------------------------------------------------------------------
hs() -> hs(user_default).
hs(M) ->
    {ok, Path} = file:get_cwd(),
    cc(M, [{outdir, Path ++ "/lib/" ++ atom_to_list(pj()) ++ "-" ++ vs() ++ "/ebin"}]).

cc(M, L) ->
    code:ensure_loaded(M),
    F = element(2, lists:keyfind(source, 1, erlang:get_module_info(M, compile))),
    Path = binary_to_list(hd(re:split(F, "/src/"))),
    c:c(F, L ++ [debug_info,
                 {parse_transform, lager_transform},
                 {lager_extra_sinks, [much]},
                 {i, Path ++ "/include"}]).

cr(M) -> cc(M, [to_core]).
ca(M) -> cc(M, [to_asm]).
ce(M) -> cc(M, [export_all]).

dm(Mod) ->
    Rel = beam_lib:chunks(code:which(Mod), [abstract_code]),
    {ok, {Mod, [{abstract_code, {_, AC}} ]} } = Rel,
    Path = lists:concat([Mod, ".erl"]),
    {ok, IO} = file:open(Path, write),
    io:fwrite(IO, "~s~n", [erl_prettypr:format(erl_syntax:form_list(AC))]),
    file:close(IO).

%%------------------------------------------------------------------------------
pj() ->
    list_to_atom(element(1, init:script_id())).

vs() ->
    element(2, init:script_id()).

%%------------------------------------------------------------------------------
log(Str) -> log("log.erl", Str).
log(FileName, Str) -> io:format(element(2, file:open(FileName, [write])), "~p", [Str]).
logw(FileName, Str) -> io:format(element(2, file:open(FileName, [write])), "~w", [Str]).

pf() -> pf(3).
pf(Len) ->
    [{reduction, recon:proc_window(reductions, Len, 200)},
     {message_len, recon:proc_count(message_queue_len, Len)},
     {memory, recon:proc_count(memory, Len)},
     {cnt, recon:inet_window(cnt, Len, 200)},
     {oct, recon:inet_window(oct, Len, 3)}].

em() -> spawn(fun() -> etop:start([{output, text}, {interval, 1}, {lines, 20}, {sort, memory}]) end).
er() -> spawn(fun() -> etop:start([{output, text}, {interval, 1}, {lines, 20}, {sort, reductions}]) end).
eq() -> spawn(fun() -> etop:start([{output, text}, {interval, 1}, {lines, 20}, {sort, msg_q}]) end).
es() -> etop:stop().

%%------------------------------------------------------------------------------
fp(Fun) ->
    fprof:apply(Fun, []),
    fprof:profile(),
    fprof:analyse().

fp(Fun, FileName) ->
    fprof:apply(Fun, []),
    fprof:profile(),
    fprof:analyse({dest, FileName}).

ep(Fun) ->
    eprof:profile(Fun),
    eprof:analyze().

ep(Fun, FileName) ->
    eprof:profile(Fun),
    eprof:log(FileName),
    eprof:analyze().

-endif.

