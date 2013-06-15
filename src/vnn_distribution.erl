%%--------------------------------------------------------------------
%% @doc
%% Distribution
%% @end
%%--------------------------------------------------------------------
-module (vnn_distribution).

-export ([exponential/1]).

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
