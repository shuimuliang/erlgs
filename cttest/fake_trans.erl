-module(fake_trans).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0, send/2, history/1, clear/1, setopts/2, peername/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

send(Ref, Data) ->
    gen_server:cast(Ref, {send, Data}).

history(Ref) ->
    gen_server:call(Ref, history).

clear(Ref) ->
    gen_server:call(Ref, clear).

peername(_) -> {ok, {{127, 0, 0, 1}, 0}}.

setopts(_, _) -> ok.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start() ->
    gen_server:start(?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, []}.

handle_call(history, _From, History) ->
    {reply, History, History};

handle_call(clear, _From, _) ->
    {reply, ok, []};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send, Data}, History) ->
    {noreply, [Data | History]};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

