-module(ejabberd_online_filter_app).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1
]).

-export([
  get_env/1, set_env/2
]).

-define(APP, ejabberd_online_filter).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
  {ok, Pid } = ejabberd_online_filter_sup:start_link(),
  { ok, Pid }.

stop(_State) ->
  ok.

get_env(Key) ->
  {ok, Value } = application:get_env(?APP, Key),
  Value.

set_env(Key, Value) ->
  ok = application:set_env(?APP, Key, Value).

%%%===================================================================
%%% Internal functions
%%%===================================================================

