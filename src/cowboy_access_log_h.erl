-module(cowboy_access_log_h).
-behaviour(cowboy_stream).

-compile([{parse_transform, lager_transform}]).

-dialyzer(no_undefined_callbacks).

%% callback exports

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

-type state() :: #{
    next := any(),
    req  := cowboy_req:req(),
    sink := atom(),
    meta := #{started_at => genlib_time:ticks()}
}.

%% callbacks

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
    -> {cowboy_stream:commands(), state()}.
init(StreamID, Req, Opts) ->
    State = make_state(Req, Opts),
    {Commands0, Next} = cowboy_stream:init(StreamID, Req, Opts),
    {Commands0, State#{next => Next}}.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), State)
    -> {cowboy_stream:commands(), State} when State::state().
data(StreamID, IsFin, Data, #{next := Next0} = State) ->
    {Commands0, Next} = cowboy_stream:data(StreamID, IsFin, Data, Next0),
    {Commands0, State#{next => Next}}.

-spec info(cowboy_stream:streamid(), any(), State)
    -> {cowboy_stream:commands(), State} when State::state().
info(StreamID, {response, Code, Headers, _} = Info, #{next := Next0} = State) ->
    _ = log_access_safe(Code, Headers, State),
    {Commands0, Next} = cowboy_stream:info(StreamID, Info, Next0),
    {Commands0, State#{next => Next}};
info(StreamID, Info, #{next := Next0} = State) ->
    {Commands0, Next} = cowboy_stream:info(StreamID, Info, Next0),
    {Commands0, State#{next => Next}}.

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), state()) -> any().
terminate(StreamID, Reason, #{next := Next}) ->
    cowboy_stream:terminate(StreamID, Reason, Next).

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(),
    cowboy_stream:partial_req(), Resp, cowboy:opts()) -> Resp
    when Resp::cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, {_, Code, Headers, _} = Resp, Opts) ->
    State = make_state(PartialReq, Opts),
    _ = log_access_safe(Code, Headers, State),
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, State).

%% private functions

log_access_safe(Code, Headers, #{req := Req} = State) ->
    try
        _ = log_access(Code, Headers, State),
        Req
    catch
        Class:Reason:Stacktrace ->
            Stack = genlib_format:format_stacktrace(Stacktrace, [newlines]),
            _ = lager:error(
                  "Response hook failed for: [~p, ~p, ~p]~nwith: ~p:~p~nstacktrace: ~ts",
                  [Code, Headers, Req, Class, Reason, Stack]
                 ),
            Req
    end.

log_access(Code, Headers, #{sink := SinkName} = State) ->
    %% Call lager:log/5 here directly in order to pass request metadata (fused into
    %% lager metadata) without storing it in a process dict via lager:md/1.
    lager:log(SinkName, info, prepare_meta(Code, Headers, State), "", []).

prepare_meta(Code, Headers, #{req := Req, meta:= Meta} = _State) ->
    MD1 = set_log_meta(remote_addr, get_remote_addr(Req), lager:md()),
    MD2 = set_log_meta(peer_addr, get_peer_addr(Req), MD1),
    MD3 = set_log_meta(request_method, cowboy_req:method(Req), MD2),
    MD4 = set_log_meta(request_path, cowboy_req:path(Req), MD3),
    MD5 = set_log_meta(request_length, cowboy_req:body_length(Req), MD4),
    MD6 = set_log_meta(response_length, get_response_len(Headers), MD5),
    MD7 = set_log_meta(request_time, get_request_duration(Meta), MD6),
    MD8 = set_log_meta('http_x-request-id', cowboy_req:header(<<"x-request-id">>, Req, undefined), MD7),
    set_log_meta(status, Code, MD8).

set_log_meta(_, undefined, MD) ->
    MD;
set_log_meta(Name, Value, MD) ->
    orddict:store(Name, Value, MD).

get_peer_addr(Req) ->
    case cowboy_req:peer(Req) of
        undefined ->
            undefined;
        {IP, _Port} ->
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
    Peer  = cowboy_req:peer(Req),
    Value = cowboy_req:header(<<"x-forwarded-for">>, Req),
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

get_request_duration(Meta) ->
    case maps:get(started_at, Meta, undefined) of
        undefined ->
            undefined;
        StartTime ->
            (genlib_time:ticks() - StartTime) / 1000000
    end.

get_response_len(Headers) ->
    case maps:get(<<"content-length">>, Headers, undefined) of
        undefined ->
            undefined;
        Len ->
            genlib:to_int(Len)
    end.

make_state(Req, Opts) ->
    SinkName = maps:get(sink, Opts),
    set_meta(#{req => Req, sink => SinkName}).

set_meta(State) ->
    State#{meta => #{started_at => genlib_time:ticks()}}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

make_state(Req) ->
    make_state(Req, #{sink => dummy}).

-spec filter_meta_test() -> _.
filter_meta_test() ->
    Req = #{
        pid => self(),
        peer => undefined,
        method => <<"GET">>,
        path => <<>>,
        qs => <<>>,
        version => 'HTTP/1.1',
        headers => #{},
        host => <<>>,
        port => undefined,
        has_body => false,
        body_length => 0
    },
    State = make_state(Req),
    [
        {request_length, 0},
        {request_method, <<"GET">>},
        {request_path, <<>>},
        {request_time, _},
        {response_length, 33},
        {status, 200}
    ] = prepare_meta(200, #{<<"content-length">> => <<"33">>}, State).

-endif.
