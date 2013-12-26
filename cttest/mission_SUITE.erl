%% common_test suite for mymodule

-module(mission_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("proto_pb.hrl").
-include("pub_pb.hrl").

%% ct.
-export([all/0, suite/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% testcase
-export([ping/1]).
-export([pve_roundbegin/1]).

suite() -> [{timetrap, {seconds, 20}}].

all() -> [{group, proto}].

groups() ->
    [{proto,[
        ping,
        pve_roundbegin
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(proto, Config) ->
    start_app(),
    {Sock, Conn} = init_conn(),
    [{conn, [Conn]}, {sock, [Sock]} | Config];
init_per_group(_group, Config) ->
    Config.

end_per_group(proto, Config) ->
    stop_app(),
    Config1 = proplists:delete(conn, Config),
    Config2 = proplists:delete(sock, Config1),
    Config2;
end_per_group(_group, _Config) ->
    ok.

init_per_testcase(_testcase, Config) ->
    Config.
end_per_testcase(_testcase, Config) ->
    Socks = proplists:get_value(sock, Config),
    [fake_trans:clear(Sock) || Sock <- Socks].

ping(Config) ->
    Sock = pb_encode_ping(Config),
    timer:sleep(50),
    [Pack1] = fake_trans:history(Sock),

    RecvPack = iolist_to_binary(Pack1),
    #absmessage{action=Act, msg=SubMsg} = pub_pb:decode_absmessage(RecvPack),
    ?assert(Act=:= 'PING_RECV'),

    SubMsgDecode = proto_pb:decode_pingrecv(SubMsg),
    #pingrecv{msg = Pong} = SubMsgDecode,
    ?assert(Pong =:= "pong").

pve_roundbegin(Config) ->
    Sock = pb_encode_roundbegin(Config),
    timer:sleep(50),
    [Pack1] = fake_trans:history(Sock),

    RecvPack = iolist_to_binary(Pack1),
    ?debugVal(RecvPack),
    #absmessage{action=Act, msg=SubMsg} = pub_pb:decode_absmessage(RecvPack),
    ?assert(Act=:= 'PVE_ROUNDBEGIN_RECV'),

    SubMsgDecode = proto_pb:decode_pveroundbeginrecv(SubMsg),
    ?debugVal(SubMsgDecode),
    #pveroundbeginrecv{status = Status} = SubMsgDecode,
    ?assert(Status =:= 1).

% internal functions

start_app() ->
    application:set_env(erlgs, netconfig, ct:get_config(netconfig)),
    ok = application:start(ranch),
    ok = application:start(syntax_tools),
    ok = application:start(compiler),
    ok = application:start(goldrush),
    ok = application:start(lager),
    ok = application:start(crypto),
    ok = application:start(emysql),
    ok = application:start(erlgs),
    meck:new(ranch),
    meck:expect(ranch, accept_ack, fun(_) -> ok end),
    unlink(whereis(ranch_meck)).

stop_app() ->
    meck:unload(ranch),
    application:stop(ranch),
    application:stop(syntax_tools),
    application:stop(compiler),
    application:stop(goldrush),
    application:stop(lager),
    application:stop(crypto),
    application:stop(emysql),
    application:stop(erlgs).

init_conn() ->
    {ok, Sock} = fake_trans:start(),
    {ok, Conn} = conn_protocol:start_link(unused, Sock, fake_trans, [unused]),
    unlink(Conn),
    {Sock, Conn}.

pb_encode_ping(Config) ->
    [Conn] = proplists:get_value(conn, Config),
    [Sock] = proplists:get_value(sock, Config),
    PingMsg = <<"hello">>,
    PingPack = iolist_to_binary(proto_pb:encode_pingsend(#pingsend{msg =
                                                                   PingMsg})),
    SendPack = iolist_to_binary(pub_pb:encode_absmessage(#absmessage{
                action = 'PING_SEND',
                msg = PingPack})),
    Conn ! {tcp, Sock, SendPack},
    Sock.

pb_encode_roundbegin(Config) ->
    [Conn] = proplists:get_value(conn, Config),
    [Sock] = proplists:get_value(sock, Config),
    Level = #level{id=1},
    MsgPack = iolist_to_binary(proto_pb:encode_pveroundbeginsend(#pveroundbeginsend{level =
                                                                   Level})),
    ?debugVal(MsgPack),
    SendPack = iolist_to_binary(pub_pb:encode_absmessage(#absmessage{
                action = 'PVE_ROUNDBEGIN_SEND',
                msg = MsgPack})),
    Conn ! {tcp, Sock, SendPack},
    Sock.
