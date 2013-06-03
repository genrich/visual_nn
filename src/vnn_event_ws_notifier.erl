-module (vnn_event_ws_notifier).

-behaviour (gen_event).

-export ([add_handler/0, delete_handler/0, set_ws/1]).

-export ([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-include_lib ("lager/include/lager.hrl").

-record (state, {ws}).

add_handler () ->
    vnn_event:add_handler (?MODULE, []).

delete_handler () ->
    vnn_event:delete_handler (?MODULE, []).

set_ws (Ws) ->
    ok = gen_event:call (vnn_event, ?MODULE, {set_ws, Ws}).

init ([]) ->
    {ok, #state{}}.

handle_call({set_ws, Ws}, _State) ->
    Reply = ok,
    lager:debug ("setting ws = ~p", [Ws]),
    {ok, Reply, #state{ws = Ws}}.

handle_event ({send_event, Arg}, #state{ws = Ws} = State) ->
    yaws_api:websocket_send (Ws, {binary, <<Arg:32/little-signed-integer>>}),
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
