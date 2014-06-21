%%--------------------------------------------------------------------------------------------------
%% @doc
%% Yaws supervisor
%% @end
%%--------------------------------------------------------------------------------------------------
-module (vnn_yaws_sup).

-export ([start_link/0, start_children/0]).

-behaviour (supervisor).
-export ([init/1]).

-define (SERVER, ?MODULE).

%%--------------------------------------------------------------------------------------------------
%% @doc
%% Start yaws supervisor
%% @end
%%--------------------------------------------------------------------------------------------------
-spec start_link () -> {ok, pid ()} | ignore | {error, {already_started, pid ()} | {shutdown, term ()} | term ()}.
%%--------------------------------------------------------------------------------------------------
start_link () ->
    supervisor:start_link ({local, ?SERVER}, ?MODULE, []).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Start yaws supervisor children
%% @end
%%--------------------------------------------------------------------------------------------------
-spec start_children () -> ok.
%%--------------------------------------------------------------------------------------------------
start_children () ->
    Id        = "embedded_yaws",
    GConfList = [{id, Id}],
    Docroot   = "./www",
    SConfList = [{port, 8080},
                 {servername, Id},
                 {listen, {0, 0, 0, 0}},
                 {docroot, Docroot},
                 {flags, [{access_log, false}, {auth_log, false}]}],

    {ok, SCList, GC, ChildSpecs} = yaws_api:embedded_start_conf (Docroot, SConfList, GConfList, Id),
    [supervisor:start_child (?SERVER, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf (GC, SCList),
    ok.


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Initialize supervision tree
%% @end
%%--------------------------------------------------------------------------------------------------
-spec init ([]) -> {ok, {{one_for_all, non_neg_integer (), non_neg_integer ()}, []}}.
%%--------------------------------------------------------------------------------------------------
init ([]) ->
    {ok, {{one_for_all, 1, 10}, []}}.
