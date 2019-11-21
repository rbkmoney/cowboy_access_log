-module(cowboy_access_log_h).
-behaviour(cowboy_stream).

-dialyzer(no_undefined_callbacks).

-type extra_info_fun() :: fun((cowboy_req:req()) -> #{atom() => term()}).
-export_type([extra_info_fun/0]).

%% API exports

-export([set_extra_info_fun/2]).

%% callback exports

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

-type state() :: #{
    next := any(),
    req  := cowboy_req:req(),
    meta := #{started_at => genlib_time:ts()},
    ext_fun := extra_info_fun()
}.

%% API

-spec set_extra_info_fun(extra_info_fun(), cowboy:opts())
   -> cowboy:opts().
set_extra_info_fun(Fun, Opts) when is_function(Fun, 1) ->
    Opts#{extra_info_fun => Fun}.

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
info(StreamID, {IsResponse, Code, Headers, _} = Info, #{req := Req, next := Next0} = State) when
    IsResponse == response;
    IsResponse == error_response
->
    _ = log_access_safe(Code, Headers, State, get_request_body_length(Req)),
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

%% NOTE: in early_error cowboy uses PartialReq, a cowboy_req:req() - like structure
%% for more info see https://ninenines.eu/docs/en/cowboy/2.7/manual/cowboy_stream/#_callbacks

early_error(StreamID, Reason, PartialReq, {_, Code, Headers, _} = Resp, Opts) ->
    State = make_state(PartialReq, Opts),
    _ = log_access_safe(Code, Headers, State, undefined),
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, State).

%% private functions

log_access_safe(Code, Headers, #{req := Req} = State, ReqBodyLength) ->
    try
        logger:log(info, "", [], prepare_meta(Code, Headers, State, ReqBodyLength)),
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
prepare_meta(Code, Headers, #{req := Req, meta:= Meta0, ext_fun := F}, ReqBodyLength) ->
    AccessMeta = genlib_map:compact(#{
        domain              => [cowboy_access_log],
        status              => Code,
        remote_addr         => get_remote_addr(Req),
        peer_addr           => get_peer_addr(Req),
        request_method      => cowboy_req:method(Req),
        request_path        => cowboy_req:path(Req),
        request_length      => ReqBodyLength,
        response_length     => get_response_len(Headers),
        request_time        => get_request_duration(Meta0),
        'http_x-request-id' => cowboy_req:header(<<"x-request-id">>, Req, undefined)
    }),
    AccessMeta1 = maps:merge(get_process_meta(), AccessMeta),
    maps:merge(F(Req), AccessMeta1).

get_request_body_length(Req) ->
    case cowboy_req:has_body(Req) of
        false -> undefined;
        true -> cowboy_req:body_length(Req)
    end.

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

make_state(Req, Opts) ->
    ExtFun = make_ext_fun(Opts),
    set_meta(#{req => Req, ext_fun => ExtFun}).

set_meta(State) ->
    State#{meta => #{started_at => genlib_time:ticks()}}.

make_ext_fun(Opts) ->
    maps:get(extra_info_fun, Opts, fun(_Req) -> #{} end).

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
        has_body => true
    },
    State = make_state(Req, #{}),
    #{
        request_method := <<"GET">>,
        request_path := <<>>,
        request_time := _,
        response_length := 33,
        request_length := 100,
        peer_addr := <<"42.42.42.42">>,
        status := 200
    } = prepare_meta(200, #{<<"content-length">> => <<"33">>}, State, 100).

-spec filter_meta_for_error_test() -> _.
filter_meta_for_error_test() ->
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
        has_body => true
    },
    State = make_state(Req, #{}),
    #{
        peer_addr := <<"42.42.42.42">>,
        remote_addr := <<"42.42.42.42">>,
        request_method := <<"GET">>,
        request_path := <<>>,
        request_time := _,
        status := 400
    } = prepare_meta(400, #{}, State, undefined).

-endif.
