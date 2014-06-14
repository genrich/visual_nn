%%--------------------------------------------------------------------
%% @doc
%% Distribution
%% @end
%%--------------------------------------------------------------------
-module (vnn_random).

-export ([exponential/1, normal/2, uniform/0, uniform/2]).

-on_load (init/0).

-include_lib ("lager/include/lager.hrl").

-define (APPNAME, visual_nn).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load NIF
%% @end
%%--------------------------------------------------------------------
-spec init () -> ok.

init () ->
    case code:priv_dir (?APPNAME) of
        {error, _} -> 
            lager:error ("~w priv dir not found", [?APPNAME]),
            exit (error);
        PrivDir ->
            erlang:load_nif (filename:join ([PrivDir, ?MODULE]), 0)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Exponential distribution for rate lambda
%% @end
%%--------------------------------------------------------------------
-spec exponential (Lambda :: float ()) -> float ().

exponential (_Lambda) ->
    erlang:nif_error (nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Normal distribution for rate lambda
%% @end
%%--------------------------------------------------------------------
-spec normal (Mean :: float (), Std :: float ()) -> float ().

normal (_Mean, _Std) ->
    erlang:nif_error (nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Uniform distribution [0, 1)
%% @end
%%--------------------------------------------------------------------
-spec uniform () -> float ().

uniform () ->
    erlang:nif_error (nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Uniform distribution [Min, Max)
%% @end
%%--------------------------------------------------------------------
-spec uniform (Min :: float (), Max :: float ()) -> float ().

uniform (_Min, _Max) ->
    erlang:nif_error (nif_not_loaded).

%%--------------------------------------------------------------------
-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").

exponential_test () ->
    %% ?debugFmt ("~p~n", [vnn_random:exponential (0.1)]),
    ?assert (vnn_random:exponential (0.1) > 0).

normal_test () ->
    ?assert (is_float (vnn_random:normal (0.0, 1.0))).

uniform2_test () ->
    ?assert (is_float (vnn_random:uniform (0.0, 10.0))).

uniform0_test () ->
    ?assert (is_float (vnn_random:uniform ())).

-endif.
