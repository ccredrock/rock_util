%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2017 redrock
%%% @doc base http.
%%% @end
%%%-------------------------------------------------------------------
-module(rock_http).

-export([cowboy_start/1,
         cb_check_pass/2,
         cb_check_body/1,
         cb_reply_ok/2,
         cb_reply_200/2,
         cb_reply_throw/2]).

-export([ba_heards/4,
         ba_client/1,
         ba_verify/5]).

-export([check_arg/2,
         check_arg/3,
         check_url/1]).

-export([init_httpc/1,
         httpc_request/2]).

-import(rock_util, [to_list/1]).

%%------------------------------------------------------------------------------
%% @doc cowboy
%%------------------------------------------------------------------------------
-spec cowboy_start([{Name::atom(),
                     Props::[{port, pos_integer()}
                             | {count, pos_integer()}
                             | {path, string()}
                             | {handle, atom()}]}]) -> ok.
cowboy_start([{Name, Props} | T]) ->
    Dispatch = cowboy_router:compile([{'_', [{proplists:get_value(path, Props),
                                              proplists:get_value(handle, Props),
                                              [Name, Props]}]}]),
    {ok, _} = cowboy:start_http(erlang:make_ref(),
                                proplists:get_value(count, Props),
                                [{port, proplists:get_value(port, Props)}],
                                [{env, [{dispatch, Dispatch}]}]),
    cowboy_start(T);
cowboy_start([]) -> ok.

%%------------------------------------------------------------------------------
cb_check_pass(Req, #{white := IsWhite, ba := GetSecret}) ->
    case cb_check_pass(Req, #{white => IsWhite}) of
        {ok, IP}-> {ok, IP};
        error -> cb_check_pass(Req, #{ba => GetSecret})
    end;
cb_check_pass(Req, #{white := IsWhite}) ->
    IP = cb_parse_peer(Req),
    case catch IsWhite(IP) of
        true -> {ok, IP};
        _ -> error
    end;
cb_check_pass(Req, #{ba := GetSecret}) ->
      case catch cb_parse_client(Req) of
          <<_/binary>> = Client ->
              case catch cb_ba_verify(Req, GetSecret(Client)) of
                  true -> {ok, Client};
                  _X -> error
              end;
          _ ->
              error
      end;
cb_check_pass(Req, _) -> {ok, cb_parse_peer(Req)}.

cb_parse_peer(Req) ->
    case cowboy_req:header(<<"x-real-ip">>, Req) of
        {undefined, _} ->
            {{IP, _}, _} = cowboy_req:peer(Req),
            list_to_binary(inet:ntoa(IP));
        {IP, _} ->
            IP
    end.

cb_parse_client(Req) ->
    {Auth, _}= cowboy_req:header(<<"authorization">>, Req),
    ba_client(Auth).

cb_ba_verify(Req, Secret) ->
    {Auth, _}= cowboy_req:header(<<"authorization">>, Req),
    {Method, _} = cowboy_req:method(Req),
    {Path, _} = cowboy_req:path(Req),
    {Date, _} = cowboy_req:header(<<"date">>, Req),
    ba_verify(to_list(Method), to_list(Path), to_list(Date), to_list(Auth), Secret).

cb_check_body(Req) ->
    {ok, Body, _} = cowboy_req:body(Req),
    check_body(Body).

cb_reply_ok(Req, Msg) ->
    Body = jsx:encode([{status, <<"0">>}, {message, Msg}]),
    cb_reply_200(Req, Body).

cb_reply_200(Req, Body) ->
    cb_reply(200, Body, Req).

cb_reply_throw(Req, {Error, <<>>}) ->
    Code = cb_format_code(Error),
    Message = rock_util:human_binary(Error),
    Body = jsx:encode([{status, integer_to_binary(Code)}, {message, Message}]),
    cb_reply(Code, Body, Req);
cb_reply_throw(Req, {Error, Reason}) ->
    Code = cb_format_code(Error),
    Message = iolist_to_binary([rock_util:human_binary(Error), ":", rock_util:human_binary(Reason)]),
    Body = jsx:encode([{status, integer_to_binary(Code)}, {message, Message}]),
    cb_reply(Code, Body, Req).

cb_format_code(args_error)    -> 400;
cb_format_code(auth_fail)     -> 401;
cb_format_code(version_error) -> 403;
cb_format_code(path_error)    -> 404;
cb_format_code(logic_error)   -> 500.

cb_reply(Code, Body, Req) ->
    cowboy_req:reply(Code, [{<<"content-type">>, <<"text/plain;charset=utf-8">>}], Body, Req).

%%------------------------------------------------------------------------------
%% @doc ba auth
%%------------------------------------------------------------------------------
-spec ba_heards(Method::string(), Url::string(), Client::string(), Secret::string()) -> [].
ba_heards(Method, Url, Client, Secret)
  when is_list(Method), is_list(Url), is_list(Client), is_list(Secret)->
    Date = httpd_util:rfc1123_date(),
    {ok, {_, _, _, _, Path, _}} = http_uri:parse(Url),
    Sign = ba_sign(Method, Path, Date, Secret),
    [{"Date", Date}, {"Authorization", "MWS " ++ Client ++ ":" ++ binary_to_list(Sign)}].

ba_sign(Method, Path, Date, Secret) ->
    base64:encode(crypto:hmac(sha, Secret, string:to_upper(Method) ++ " " ++ Path ++ "\n" ++ Date)).

ba_client(Auth) ->
    [<<"MWS ", Client/binary>>, _Sign] = re:split(Auth, ":"), Client.

ba_verify(Method, Path, Date, Auth, Secret) ->
    [<<"MWS ", _Client/binary>>, Sign] = re:split(Auth, ":"),
    Sign =:= ba_sign(Method, Path, Date, Secret).

%%------------------------------------------------------------------------------
%% @doc check throw
%%------------------------------------------------------------------------------
check_body(<<_/binary>> = Body) ->
    case catch jsx:decode(Body) of
        List when is_list(List) -> maps:from_list(List);
        _ -> throw({args_error, <<"no json body">>})
    end.

check_arg(<<_/binary>> = A, #{} = M) ->
    case maps:find(A, M) of
        error -> throw({args_error, A});
        {ok, V} -> V
    end.

check_arg(<<_/binary>> = A, #{} = M, Type) ->
    case check_arg(A, M) of
        V when Type =:= binary andalso not is_binary(V) -> throw({args_error, A});
        V when Type =:= non_neg_integer andalso not is_integer(V) orelse V < 0 -> throw({args_error, A});
        V when Type =:= url -> check_url(V);
        V -> V
    end.

check_url(<<_/binary>> = Url) ->
    case catch http_uri:parse(binary_to_list(Url)) of
        {ok, {_, _, Host, _Port, _, _}} when Host =/= [] -> Url;
        _R -> throw({args_error, Url})
    end.

%%------------------------------------------------------------------------------
%% @doc httpc
%%------------------------------------------------------------------------------
-define(HTTP_DNS_CACHE_TIMEOUT, 1000).
-define(HTTP_CONNECT_TIMEOUT,   1000).
-define(HTTP_QUERY_TIMEOUT,     5000).

%%------------------------------------------------------------------------------
init_httpc(ID) when is_binary(ID) ->
    inet_db:set_cache_refresh(?HTTP_DNS_CACHE_TIMEOUT),
    inet_db:set_lookup([dns]),
    Profile = binary_to_atom(rock_util:human_binary(ID), utf8),
    ProfileName = httpc:profile_name(Profile),
    whereis(ProfileName) =:= undefined andalso inets:start(httpc, [{profile, Profile}]),
    httpc_manager:set_options([{max_keep_alive_length, 0}], ProfileName),
    {_, _, HanlderETS, _, SessionETS, _, _} = sys:get_state(PID = whereis(ProfileName)),
    put({?MODULE, httpc_profile}, PID),
    #{profile => PID, handler => HanlderETS, session => SessionETS}.

-spec httpc_request(Url::string(), Secret::#{}) ->
    {ok, any()} | {ok, any(), any()} | {ok, any(), any(), any()} | {error, any()}.
httpc_request(Url, Prop) ->
    Method  = maps:get(method, Prop, post),
    Profile = case maps:get(profile, Prop, get({?MODULE, httpc_profile})) of undefined -> default; V -> V end,
    Sync    = maps:get(sync, Prop, true),
    Body    = maps:get(body, Prop, <<>>),
    Head = [{"User-Agent", "rock_util/1.0"} | form_headers(Url, Method, Prop)],
    case catch httpc:request(Method,
                             {Url, Head, "application/json", Body},
                             [{connect_timeout, maps:get(connect_timeout, Prop, ?HTTP_CONNECT_TIMEOUT)},
                              {timeout, maps:get(query_timeout, Prop, ?HTTP_QUERY_TIMEOUT)}],
                             [{sync, Sync}],
                             Profile) of
        {ok, _, _, _} = Result -> Result;
        {ok, _, _} = Result -> Result;
        {ok, _} = Result -> Result;
        {_, Reason} -> {error, Reason}
    end.

%% @private
form_headers(Url, Method, #{client := Client, secret := Secret}) ->
    MethodStr = string:to_upper(atom_to_list(Method)),
    ba_heards(MethodStr, Url, Client, Secret);
form_headers(_Url, _Method, #{}) ->
    Date = httpd_util:rfc1123_date(),
    [{"Date", Date}].

