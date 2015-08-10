%%%-------------------------------------------------------------------
%% @doc foxplan public API
%% @end
%%%-------------------------------------------------------------------

-module(foxplan_app).
-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/ws/web", web_handler, []},
      {"/ws/mobile", mobile_handler, []}
    ]}
  ]),

  PortOption = {port, 8080},
  DispatchOption = {dispatch, Dispatch},
  cowboy:start_http(http, 100, [PortOption], [{env, [DispatchOption]}]),

  foxplan_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.
