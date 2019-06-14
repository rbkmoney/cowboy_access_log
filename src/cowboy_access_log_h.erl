-module(cowboy_access_log_h).
-behaviour(cowboy_stream).

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
    meta := #{started_at => genlib_time:ts()}
}.

%% callbacks

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
    -> {cowboy_stream:commands(), state()}.
init(StreamID, Req, Opts) ->
    State = make_state(Req),
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
early_error(StreamID, Reason, PartialReq, {_, Code, Headers, _} = Resp, _Opts) ->
    State = make_state(PartialReq),
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
            _ = logger:error(
                  "Log access failed for: [~p, ~p, ~p]~nwith: ~p:~p~nstacktrace: ~ts",
                  [Code, Headers, Req, Class, Reason, Stack]
                 ),
            Req
    end.

log_access(Code, Headers, State) ->
    logger:log(info, "", [], prepare_meta(Code, Headers, State)).

get_process_meta() ->
    case logger:get_process_metadata() of
        undefined ->
            #{};
        Meta ->
            Meta
    end.

% domain field specifies the functional area that send log event
% as we want to save logs from this app in a separate file,
% we can easily filter logs by their domain using OTP filter functions.
prepare_meta(Code, Headers, #{req := Req, meta:= Meta0} = _State) ->
    AccessMeta = genlib_map:compact(#{
        domain              => [cowboy_access_log],
        status              => Code,
        remote_addr         => get_remote_addr(Req),
        peer_addr           => get_peer_addr(Req),
        request_method      => cowboy_req:method(Req),
        request_path        => cowboy_req:path(Req),
        request_length      => cowboy_req:body_length(Req),
        response_length     => get_response_len(Headers),
        request_time        => get_request_duration(Meta0),
        'http_x-request-id' => cowboy_req:header(<<"x-request-id">>, Req, undefined)
    }),
    maps:merge(get_process_meta(), AccessMeta).

get_peer_addr(Req) ->
    {IP, _Port} = cowboy_req:peer(Req),
    genlib:to_binary(inet:ntoa(IP)).

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
    end.

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

make_state(Req) ->
    set_meta(#{req => Req}).

set_meta(State) ->
    State#{meta => #{started_at => genlib_time:ticks()}}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec filter_meta_test() -> _.
filter_meta_test() ->
    Req = #{
        pid => self(),
        peer => {{42, 42, 42, 42}, 4242},
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
    #{
        request_length := 0,
        request_method := <<"GET">>,
        request_path := <<>>,
        request_time := _,
        response_length := 33,
        peer_addr := <<"42.42.42.42">>,
        status := 200
    } = prepare_meta(200, #{<<"content-length">> => <<"33">>}, State).

-endif.
