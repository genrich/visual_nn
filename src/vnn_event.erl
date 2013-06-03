-module (vnn_event).

-export ([start_link/0, add_handler/2, delete_handler/2, send_event/1]).

start_link () ->
    gen_event:start_link ({local, ?MODULE}).

add_handler (Handler, Args) ->
    gen_event:add_handler (?MODULE, Handler, Args).

delete_handler (Handler, Args) ->
    gen_event:delete_handler (?MODULE, Handler, Args).

send_event (Arg) ->
    gen_event:notify (?MODULE, {send_event, Arg}).
