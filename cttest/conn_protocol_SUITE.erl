%% common_test suite
%%
-module(conn_protocol_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("pub_pb.hrl").
-include("proto_pb.hrl").

%% ct.
-export([all/0, suite/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% tcp
-export([tcp_ping/1]).

suite() -> [{timetrap, {seconds, 20}}].

all() -> [{group, tcp}].

groups() ->
    [{tcp,[
        tcp_ping
        ]}
    ].

init_per_suite(Config) ->
    ok = application:start(ranch),
    Config.

end_per_suite(_Config) ->
    application:stop(ranch),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

pb_encode_ping() ->
    Send = proto_pb:encode_pingsend(#pingsend{msg = "ping"}),
    pub_pb:encode_absmessage(#absmessage{
                        action = 'PING_SEND',
                        msg = iolist_to_binary(Send)
    }).

tcp_ping(_Config) ->
    NetConfig = ct:get_config(netconfig),
    AcceptorNum = proplists:get_value(acceptornum, NetConfig),
    Host = proplists:get_value(host, NetConfig),
    Opts = proplists:get_value(opts, NetConfig),

    % select random port when port = 0

    Listener = tcp_ping,
    {ok, _} = ranch:start_listener(Listener, AcceptorNum, ranch_tcp,
                            [{host, Host}, {port, 0}], conn_protocol, [Opts]),
    Port = ranch:get_port(Listener),

    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false},
                                                {packet, 4}]),
    ok = gen_tcp:send(Socket, pb_encode_ping()),

    {ok, RecvData} = gen_tcp:recv(Socket, 0, 1000),

    ABSMsg = pub_pb:decode_absmessage(RecvData),

    #absmessage{action = Act, msg = SubMsg} = ABSMsg,
    ?assert(Act=:= 'PING_RECV'),

    SubMsgDecode = proto_pb:decode_pingrecv(SubMsg),
    #pingrecv{msg = Pong} = SubMsgDecode,
    ?assert(Pong =:= "pong"),

    ok = ranch:stop_listener(Listener),
    {error, closed} = gen_tcp:recv(Socket, 0, 1000),

    %% Make sure the listener stopped.
    {'EXIT', _} = begin catch ranch:get_port(Listener) end,
    ok.
