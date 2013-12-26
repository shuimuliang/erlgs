-module(conn_protocol_tests).
-include_lib("eunit/include/eunit.hrl").

send_to_recv_test() ->
    Act = 'PVE_ROUNDBEGIN_SEND',
    Result = 'PVE_ROUNDBEGIN_RECV',
    R = conn_protocol:send_to_recv(Act),
    ?assert(Result =:= R).

act_to_decode_send_test() ->
    Act = 'PVE_ROUNDBEGIN_SEND',
    Result = decode_pveroundbeginsend,
    R = conn_protocol:act_to_decode_send(Act),
    ?assert(Result =:= R).

act_to_encode_recv_test() ->
    Act = 'PVE_ROUNDBEGIN_SEND',
    Result = encode_pveroundbeginrecv,
    R = conn_protocol:act_to_encode_recv(Act),
    ?assert(Result =:= R).

act_to_fun_test() ->
    Act = 'PVE_ROUNDBEGIN_SEND',
    Result = pve_roundbegin,
    R = conn_protocol:act_to_fun(Act),
    ?assert(Result =:= R).
