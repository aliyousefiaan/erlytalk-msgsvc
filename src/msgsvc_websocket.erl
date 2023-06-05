-module(msgsvc_websocket).

-include("records.hrl").

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, _State) ->
    #{pid := Pid, headers := Headers, qs := QS} = Req,
    UserId =
        case Headers of
            #{<<"userid">> := HUserId} ->
                HUserId;
            _ ->
                <<"userid=", QSUserId/binary>> = QS,
                QSUserId
        end,
    {cowboy_websocket, Req, #ws_session{
        user_id = UserId,
        pid = Pid,
        timestamp = os:system_time(millisecond)
    }}.

websocket_init(State) ->
    io:format("WebSocket initial (~p): ~p", [self(), State]),
    case syn:lookup(ws_session, State#ws_session.user_id) of
        undefined ->
            ok = syn:register(ws_session, State#ws_session.user_id, State#ws_session.pid, [
                {timestamp, State#ws_session.timestamp}
            ]);
        _ ->
            ok = syn:unregister(ws_session, State#ws_session.user_id),
            ok = syn:register(ws_session, State#ws_session.user_id, State#ws_session.pid, [
                {timestamp, State#ws_session.timestamp}
            ])
    end,
    {ok, State}.

websocket_handle({text, InFrame}, State) ->
    JInFrame = jsx:decode(InFrame, []),
    io:format("WebSocket handling frame: ~p", [JInFrame]),
    dispatch_frame(JInFrame, State);
websocket_handle(_InFrame, State) ->
    {[{text, <<"error-unknown-frame">>}], State}.

websocket_info({send, Msg}, State) ->
    {[{text, jsx:encode(Msg)}], State};
websocket_info(_Msg, State) ->
    {ok, State}.

terminate(_Reason, _PartialReq, _State) ->
    ok.

dispatch_frame(
    #{
        <<"frameType">> := <<"sendMessage">>,
        <<"destination">> := Destination,
        <<"body">> := Body
    },
    State
) ->
    UserId = State#ws_session.user_id,

    MsgEvent = #{
        <<"frameType">> => <<"msgEvent">>,
        <<"senderId">> => UserId,
        <<"body">> => Body
    },

    case syn:lookup(ws_session, Destination) of
        {Pid, _} ->
            Pid ! {send, MsgEvent};
        undefined ->
            ok
    end,

    {[{text, <<"ack">>}], State};
dispatch_frame(_, State) ->
    {[{text, not_implemented}], State}.
