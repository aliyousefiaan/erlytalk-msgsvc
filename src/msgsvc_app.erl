%%%-------------------------------------------------------------------
%% @doc msgsvc public API
%% @end
%%%-------------------------------------------------------------------

-module(msgsvc_app).

-include("records.hrl").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = start_http_server(),
    msgsvc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
start_http_server() ->
    ok = syn:add_node_to_scopes([ws_session]),

    {ok, BindAddress} = inet_parse:address(msgsvc:conf(http_bind_addr)),
    BindPort = list_to_integer(msgsvc:conf(http_bind_port)),

    Dispatch = cowboy_router:compile(
        [
            {'_', [
                {"/", cowboy_static, {priv_file, msgsvc, "index.html"}},
                {"/ws", msgsvc_websocket, []}
            ]}
        ]
    ),
    {ok, _} = cowboy:start_clear(
        msgsvc,
        [{ip, BindAddress}, {port, BindPort}],
        #{
            env => #{dispatch => Dispatch}
        }
    ),
    io:format("HTTP server started on port ~p~n", [BindPort]),

    ok.
