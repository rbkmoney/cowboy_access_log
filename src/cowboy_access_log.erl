-module(cowboy_access_log).

-compile([{parse_transform, lager_transform}]).

%% API exports
-export([get_request_hook/0]).
-export([get_response_hook/1]).

-define(START_TIME_TAG, cowboy_access_log_request_handling_started_at).

%%====================================================================
%% API functions
%%====================================================================
-spec get_request_hook() ->
    cowboy:onrequest_fun().
get_request_hook() ->
    fun(Req) -> request_hook(Req) end.

-spec get_response_hook(atom()) ->
    cowboy:onresponse_fun().
get_response_hook(Sinkname) ->
    fun(Code, Headers, IO, Req) ->
        response_hook(Sinkname, Code, Headers, IO, Req)
    end.

%%====================================================================
%% Internal functions
%%====================================================================
request_hook(Req) ->
    set_meta(Req).

-spec response_hook(atom(), cowboy:http_status(), cowboy:http_headers(), iodata(), cowboy_req:req()) ->
    cowboy_req:req().

response_hook(SinkName, Code, Headers, _, Req) ->
    try
        _ = log_access(SinkName, Code, Headers, Req),
        Req
    catch
        Class:Reason ->
            Stack = genlib_format:format_stacktrace(erlang:get_stacktrace(), [newlines]),
            _ = lager:error(
                  "Response hook failed for: [~p, ~p, ~p]~nwith: ~p:~p~nstacktrace: ~ts",
                  [Code, Headers, Req, Class, Reason, Stack]
                 ),
            Req
    end.

log_access(SinkName, Code, Headers, Req) ->
    %% Call lager:log/5 here directly in order to pass request metadata (fused into
    %% lager metadata) without storing it in a process dict via lager:md/1.
    lager:log(SinkName, info, prepare_meta(Code, Headers, Req), "", []).

-spec set_meta(cowboy_req:req()) ->
    cowboy_req:req().
set_meta(Req) ->
    cowboy_req:set_meta(?START_TIME_TAG, genlib_time:ticks(), Req).

prepare_meta(Code, Headers, Req) ->
    MD1 = set_log_meta(remote_addr,         get_remote_addr(Req),           lager:md()),
    MD2 = set_log_meta(peer_addr,           get_peer_addr(Req),             MD1),
    MD3 = set_log_meta(
        request_method,
        unwrap_cowboy_result(cowboy_req:method(Req)),
        MD2
    ),
    MD4 = set_log_meta(
        request_path,
        unwrap_cowboy_result(cowboy_req:path(Req)),
        MD3),
    MD5 = set_log_meta(
        request_length,
        unwrap_cowboy_result(cowboy_req:body_length(Req)),
        MD4),
    MD6 = set_log_meta(response_length,     get_response_len(Headers),      MD5),
    MD7 = set_log_meta(request_time,        get_request_duration(Req),      MD6),
    MD8 = set_log_meta(
        'http_x-request-id',
        unwrap_cowboy_result(cowboy_req:header(<<"x-request-id">>, Req, undefined)),
        MD7
    ),
    set_log_meta(status, Code, MD8).

set_log_meta(_, undefined, MD) ->
    MD;
set_log_meta(Name, Value, MD) ->
    orddict:store(Name, Value, MD).

unwrap_cowboy_result({Value, _}) ->
    Value.

get_peer_addr(Req) ->
    case cowboy_req:peer(Req) of
        {undefined, _} ->
            undefined;
        {{IP, _Port}, _Req1} ->
            genlib:to_binary(inet:ntoa(IP))
    end.

get_remote_addr(Req) ->
    case determine_remote_addr(Req) of
        {ok, RemoteAddr} ->
            genlib:to_binary(inet:ntoa(RemoteAddr));
        _ ->
            undefined
    end.

determine_remote_addr(Req) ->
    {Peer, Req1}  = cowboy_req:peer(Req),
    {Value, _Req2} = cowboy_req:header(<<"x-forwarded-for">>, Req1),
    determine_remote_addr_from_header(Value, Peer).

determine_remote_addr_from_header(undefined, {IP, _Port}) ->
    % undefined, assuming no proxies were involved
    {ok, IP};
determine_remote_addr_from_header(Value, _Peer) when is_binary(Value) ->
    ClientPeer = string:strip(binary_to_list(Value)),
    case string:tokens(ClientPeer, ", ") of
        [ClientIP | _Proxies] ->
            inet:parse_strict_address(ClientIP);
        _ ->
            {error, malformed}
    end;
determine_remote_addr_from_header(undefined, undefined) ->
    {error, undefined}.

get_request_duration(Req) ->
    case cowboy_req:meta(?START_TIME_TAG, Req) of
        {undefined, _} ->
            undefined;
        {StartTime, _} ->
            (genlib_time:ticks() - StartTime) / 1000000
    end.

get_response_len(Headers) ->
    case lists:keyfind(<<"content-length">>, 1, Headers) of
        {_, Len} ->
            genlib:to_int(Len);
        false ->
            undefined
    end.

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec filter_meta_test() -> _.
filter_meta_test() ->
    Req = cowboy_req:new(
        undefined, undefined, undefined, <<"GET">>, <<>>, <<>>,
        'HTTP/1.1', [], <<>>, undefined, <<>>, false, false, undefined
    ),
    [
        {request_length, 0},
        {request_method, <<"GET">>},
        {request_path, <<>>},
        {status, 200}
    ] = prepare_meta(200, [], Req).

-endif.
