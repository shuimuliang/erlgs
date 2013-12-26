%% -*- coding: utf-8 -*-
%% TCP 接入和转码

-module(conn_protocol).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("conn_protocol.hrl").
-include("proto_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/4, abs_pack/2, act_to_fun/1,
         send_to_recv/1, act_to_decode_send/1, act_to_encode_recv/1]).

-export([ping/2, pve_roundbegin/2, pve_roundend/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% ranch_protocol Function Definitions
%% ------------------------------------------------------------------

start_link(Ref, Socket, Transport, [Opts]) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Ref, Socket, Transport, Opts], []),
    Pid ! init,
    {ok, Pid}.

init(Args) ->
    process_flag(trap_exit, true),
    {ok, [init | Args]}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% really init things
handle_info(init, [init, Ref, Socket, Transport, Opts]) ->
    ok = proc_lib:init_ack({ok, self()}),
    {ok, {IHost, Port}} = Transport:peername(Socket),
    Host = inet_parse:ntoa(IHost),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, true} | Opts]),
    {noreply, #connstate{socket=Socket, host=Host, port=Port, transport=Transport}};

handle_info({tcp, Sock, Data}, #connstate{transport=Transport}=State) ->
    case handler(Data, State) of
        error -> {noreply, State};
        {noreply, NewState} -> {noreply, NewState};
        {SendData, NewState} ->
            io:format(user, "Send: ~p~n", [SendData]),
            Transport:send(Sock, SendData),
            {noreply, NewState}
    end;

handle_info({tcp_closed, _Sock},
            #connstate{host=Host, port=Port}=State) ->
    lager:info("Socket disconnected from ~s:~p~n", [Host, Port]),
    {stop, normal, State};

handle_info({tcp_error, _Sock, Reason},
            #connstate{host=Host, port=Port}=State) ->
    lager:warning("Socket error from ~s:~p, error : ~p~n", [Host, Port, Reason]),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

ping(#pingsend{msg=_Msg}, State) ->
    SendMsg = #pingrecv{msg = <<"pong">>},
    {SendMsg, State}.

pve_roundbegin(#pveroundbeginsend{level=Level}, State) ->
    SendMsg = #pveroundbeginrecv{status=1},
    {SendMsg, State}.

pve_roundend(#pveroundendsend{}, State) ->
    SendMsg = #pveroundendrecv{},
    {SendMsg, State}.

handler(Data, State) ->
    try pub_pb:decode_absmessage(Data) of
        #absmessage{action=Act, msg=Msg} ->
            io:format(user, "Recv, Act: ~p, Msg: ~p~n", [Act, Msg]),
            Send = act_to_decode_send(Act),
            DecodedMsg = apply(proto_pb, Send, [Msg]),
            case apply(?MODULE, act_to_fun(Act), [DecodedMsg, State]) of
                {noreply, NewState} -> {noreply, NewState};
                {Result, NewState} ->
                    Recv = act_to_encode_recv(Act),
                    RMsg = apply(proto_pb, Recv, [Result]),
                    {abs_pack(send_to_recv(Act), RMsg), NewState}
            end
    catch
        error:_ -> error
    end.

abs_pack(Act, Data) ->
    pub_pb:encode_absmessage(#absmessage{action=Act, msg=Data}).

act_to_decode_send(Act) ->
    ActStr = atom_to_list(Act),
    Tokens = [string:to_lower(X) || X <- string:tokens(ActStr, "_")],
    SendStr = string:join(["decode_" | Tokens], ""),
    list_to_atom(SendStr).

act_to_encode_recv(Act) ->
    ActStr = atom_to_list(send_to_recv(Act)),
    Tokens = [string:to_lower(X) || X <- string:tokens(ActStr, "_")],
    RecvStr = string:join(["encode_" | Tokens], ""),
    list_to_atom(RecvStr).

send_to_recv(Act) ->
    ActStr = atom_to_list(Act),
    Len = length(ActStr),
    Pre = string:substr(ActStr, 1, Len - 4),
    list_to_atom(Pre ++ "RECV").

act_to_fun(Act) ->
    ActStr = atom_to_list(Act),
    Len = length(ActStr),
    Pre = string:substr(ActStr, 1, Len - 5),
    list_to_atom(string:to_lower(Pre)).
