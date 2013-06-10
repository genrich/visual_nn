%%--------------------------------------------------------------------
%% @doc
%% Stimulus
%% @end
%%--------------------------------------------------------------------
-module (vnn_stimulus).

-export ([start/0, loop/1]).

-include_lib ("lager/include/lager.hrl").

-record (state, {network  :: pid (),
                 id       :: non_neg_integer (),
                 active   :: boolean (),
                 position :: vnn_network:position ()}).

-define (STRIDE, 40).
-define (LINES,  20).
-define (STEP,   20).

%%--------------------------------------------------------------------
%% @doc
%% Start stimulus array
%% @end
%%--------------------------------------------------------------------
-spec start () -> [pid ()].

start () ->
    X =
        "                                        "
        "                                        "
        "    X   X  XXXXX  X      X       XXX    "
        "    X   X  X      X      X      X   X   "
        "    X   X  X      X      X      X   X   "
        "    XXXXX  XXX    X      X      X   X   "
        "    X   X  X      X      X      X   X   "
        "    X   X  X      X      X      X   X   "
        "    X   X  XXXXX  XXXXX  XXXXX   XXX    "
        "                                        "
        "    X   X   XXX   XXXX   X      XXX     "
        "    X   X  X   X  X   X  X      X  X    "
        "    X   X  X   X  X   X  X      X   X   "
        "    X X X  X   X  XXXX   X      X   X   "
        "    X X X  X   X  X X    X      X   X   "
        "    XX XX  X   X  X  X   X      X  X    "
        "    X   X   XXX   X   X  XXXXX  XXX     "
        "                                        "
        "                                        "
        "                                        ",
    {Pids, _} =
        lists:mapfoldl (fun ($X, {I0, ?STRIDE, ?STRIDE}) -> I = I0+1, J = 0, {start (I, J, true),  {I, J+1, ?STRIDE}};
                            ($X, {I,        J, ?STRIDE}) ->                  {start (I, J, true),  {I, J+1, ?STRIDE}};
                            ($ , {I0, ?STRIDE, ?STRIDE}) -> I = I0+1, J = 0, {start (I, J, false), {I, J+1, ?STRIDE}};
                            ($ , {I,        J, ?STRIDE}) ->                  {start (I, J, false), {I, J+1, ?STRIDE}} end,
                        {0, 0, ?STRIDE}, X),
    Pids.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Start stimulus unit
%% @end
%%--------------------------------------------------------------------
-spec start (I :: non_neg_integer (), J :: non_neg_integer (), Active :: boolean ()) -> pid ().

start (I, J, Active) ->
    erlang:spawn (?MODULE, loop, [#state{network  = self (),
                                         id       = I * ?STRIDE + J,
                                         active   = Active,
                                         position = {I * ?STEP - ?LINES * ?STEP / 2 + ?STEP / 2,
                                                     -300.0,
                                                     J * ?STEP - ?STRIDE * ?STEP / 2 + ?STEP / 2}}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec loop (#state{}) -> no_return ().

loop (#state{network = _NetworkPid, id = Id, active = Active, position = Position} = State) ->
    receive
        sim_start -> vnn_event:send_stimulus_pos (Id, Position);
        sim_stop  -> ok;
        {is_active, Pid} -> Pid ! Active;
        {position,  Pid} -> Pid ! Position
    end,
    vnn_stimulus:loop (State).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------
-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").

is_active (Pid) -> Pid ! {is_active, self ()}, receive X -> X end.

position (Pid) -> Pid ! {position, self ()}, receive X -> X end.

spawn_test () ->
    ?assert (is_pid (start (0, 0, true))),
    Pids = start (),
    ?assertEqual (?STRIDE * ?LINES, length (Pids)),
    ?assert (is_pid (lists:nth (1, Pids))),
    ?assertNot (is_active (lists:nth (1, Pids))),
    ?assertEqual ({-190.0, -300.0, -390.0}, position (lists:nth (1, Pids))),
    ?assertEqual ({190.0, -300.0, 390.0}, position (lists:nth (?STRIDE * ?LINES, Pids))),
    ?assert (is_active (lists:nth (125, Pids))).

-endif.
