-module(erlgs_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, AppEnv} = application:get_key(erlgs, env),

    % net
    NetConfig = proplists:get_value(netconfig, AppEnv),
    AcceptorNum = proplists:get_value(acceptornum, NetConfig),
    Host = proplists:get_value(host, NetConfig),
    Port = proplists:get_value(port, NetConfig),
    Opts = proplists:get_value(opts, NetConfig),

    {ok, _} = ranch:start_listener(erlgs, AcceptorNum, ranch_tcp,
        [
            {host, Host},
            {port, Port},
            {max_connections, infinity}
        ],
        conn_protocol, [Opts]),

    erlgs_sup:start_link().

stop(_State) ->
    ok.
