%%%-------------------------------------------------------------------
%% @doc foxplan top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('foxplan_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
  Children = [
    ?CHILD(doorman_server, worker)
  ],

  {ok, { {one_for_all, 0, 1}, Children} }.
