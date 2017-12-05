-module(cowboy_access_log).

%% API exports
-export([request_hook /1]).
-export([response_hook/5]).

-define(START_TIME_TAG, cowboy_access_log_request_handling_started_at).

%%====================================================================
%% API functions
%%====================================================================
-spec request_hook(cowboy_req:req()) ->
    cowboy_req:req().

request_hook(Req) ->
    set_meta(Req).

-spec response_hook(atom(), cowboy:http_status(), cowboy:http_headers(), iodata(), cowboy_req:req()) ->
    cowboy_req:req().

response_hook(SinkName, Code, Headers, _, Req) ->
    try
        {Code1, Headers1, Req1} = handle_response(Code, Headers, Req),
        _ = log_access(SinkName, Code1, Headers1, Req1),
        Req1
    catch
        Class:Reason ->
            Stack = genlib_format:format_stacktrace(erlang:get_stacktrace(), [newlines]),
            _ = lager:error(
                    "Response hook failed for: [~p, ~p, ~p]~nwith: ~p:~p~nstacktrace: ~ts",
                    [Code, Headers, Req, Class, Reason, Stack]
                ),
            Req
    end.

%%====================================================================
%% Internal functions
%%====================================================================
handle_response(Code, Headers, Req) ->
    {Code, Headers, Req}.

log_access(SinkName, Code, Headers, Req) ->
    {Method, _} = cowboy_req:method(Req),
    {Path,   _} = cowboy_req:path(Req),
    {ReqLen, _} = cowboy_req:body_length(Req),
    {ReqId,  _} = cowboy_req:header(<<"x-request-id">>, Req, undefined),
    {RemAddr, PeerAddr} = get_remote_addr(Req),
    RespLen  = get_response_len(Headers),
    Duration = get_request_duration(Req),
    ReqMeta = [
        {rem_addr, RemAddr},
        {peer_addr, PeerAddr},
        {request_method, Method},
        {request_path, Path},
        {request_length, ReqLen},
        {response_length, RespLen},
        {request_time, Duration},
        {'http_x-request-id', ReqId},
        {status, Code}
    ],
    Meta = orddict:merge(fun(_Key, New, _Old) -> New end, ReqMeta, lager:md()),
    %% Call lager:log/5 here directly in order to pass request metadata (fused into
    %% lager metadata) without storing it in a process dict via lager:md/1.
    lager:log(SinkName, info, Meta, "", []).

-spec set_meta(cowboy_req:req()) ->
    cowboy_req:req().
set_meta(Req) ->
    cowboy_req:set_meta(?START_TIME_TAG, genlib_time:ticks(), Req).

get_remote_addr(Req) ->
    case determine_peer(Req) of
        {{ok, #{rem_addr := RemAddr, peer_addr := PeerAddr}}, _} ->
            {genlib:to_binary(inet:ntoa(RemAddr)), genlib:to_binary(inet:ntoa(PeerAddr))};
        {_, _} ->
            undefined
    end.

determine_peer(Req) ->
    {Peer, Req1}  = cowboy_req:peer(Req),
    {Value, Req2} = cowboy_req:header(<<"x-forwarded-for">>, Req1),
    {determine_peer_from_header(Value, Peer), Req2}.

determine_peer_from_header(undefined, {IP, _Port}) ->
    % undefined, assuming no proxies were involved
    {ok, #{rem_addr => IP, peer_addr => IP}};
determine_peer_from_header(Value, {PeerIP, _Port}) when is_binary(Value) ->
    ClientPeer = string:strip(binary_to_list(Value)),
    case string:tokens(ClientPeer, ", ") of
        [ClientIP | _Proxies] ->
            case inet:parse_strict_address(ClientIP) of
                {ok, IP} ->
                    {ok, #{rem_addr => IP, peer_addr => PeerIP}};
                Error ->
                    Error
            end;
        _ ->
            {error, malformed}
    end.

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
