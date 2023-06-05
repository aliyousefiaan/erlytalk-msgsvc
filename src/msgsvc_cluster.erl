-module(msgsvc_cluster).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([start_link/0]).

-record(state, {nodes_status :: list()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, _} = timer:send_interval(15000, discover_nodes),
    {ok, #state{nodes_status = []}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(discover_nodes, State) ->
    NewState = discover_nodes(State),
    case NewState#state.nodes_status /= State#state.nodes_status of
        true ->
            io:format("New cluster nodes status ~p", [NewState#state.nodes_status]),
            {noreply, NewState};
        false ->
            {noreply, NewState}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

discover_nodes(State) ->
    NodesIP = inet_res:lookup("msgsvc-cluster.msgsvc.svc.cluster.local", in, a),
    case length(NodesIP) > 0 of
        true ->
            Nodes =
                [
                    erlang:list_to_atom(
                        lists:flatten(["msgsvc@", inet:ntoa(A)])
                    )
                 || A <- NodesIP
                ],
            NodesStatus = [{Node, net_kernel:connect_node(Node)} || Node <- Nodes, Node =/= node()],
            State#state{nodes_status = NodesStatus};
        false ->
            State
    end.
