-module(access_lib).

%% API exports
-export([log_access/4]).
-export([set_meta  /1]).

-define(START_TIME_TAG, processing_start_time).

%%====================================================================
%% API functions
%%====================================================================
-spec log_access(atom(), pos_integer(), list(), cowboy_req:req()) ->
    ok | {error, lager_not_running}.
log_access(SinkName, Code, Headers, Req) ->
    {Method, _} = cowboy_req:method(Req),
    {Path,   _} = cowboy_req:path(Req),
    {ReqLen, _} = cowboy_req:body_length(Req),
    {ReqId,  _} = cowboy_req:header(<<"x-request-id">>, Req, undefined),
    RemAddr  = get_remote_addr(Req),
    RespLen  = get_response_len(Headers),
    Duration = get_request_duration(Req),
    ReqMeta = [
               {remote_addr, RemAddr},
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

%%====================================================================
%% Internal functions
%%====================================================================
get_remote_addr(Req) ->
    case determine_peer(Req) of
        {{ok, #{ip_address := IP}}, _} ->
            genlib:to_binary(inet:ntoa(IP));
        {_, _} ->
            undefined
    end.

determine_peer(Req) ->
    {Peer, Req1}  = cowboy_req:peer(Req),
    {Value, Req2} = cowboy_req:header(<<"x-forwarded-for">>, Req1),
    {determine_peer_from_header(Value, Peer), Req2}.

determine_peer_from_header(undefined, {IP, Port}) ->
    % undefined, assuming no proxies were involved
    {ok, #{ip_address => IP, port_number => Port}};
determine_peer_from_header(Value, _Peer) when is_binary(Value) ->
    ClientPeer = string:strip(binary_to_list(Value)),
    case string:tokens(ClientPeer, ", ") of
        [ClientIP | _Proxies] ->
            case inet:parse_strict_address(ClientIP) of
                {ok, IP} ->
                    {ok, #{ip_address => IP}};
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
