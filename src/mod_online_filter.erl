-module(mod_online_filter).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-behavior(gen_mod).
-export([
  start/2, stop/1,
  mod_opt_type/1, depends/2,
  mod_options/1
]).

%% HOOKS
-export([
  set_presence_hook/4,
  unset_presence_hook/4,
  c2s_session_pending/1
]).

%% EJABBERD_HTTP callback

-export([
  process/2
]).

-define(LAGER, true).
-include("logger.hrl").
-include("xmpp.hrl").
-include("ejabberd_http.hrl").

-define(NS_HTTP_GATEWAY, <<"urn:xmpp:http_gateway">>).
-define(XMPP_CODEC,http_gateway_xmpp).
-define(APP, -author("Sergey Loguntsov <loguntsov@gmail.com>").http_gateway).

%% gen_mod callbacks

start(Host, _Opts) ->
  { ok, _ } = application:ensure_all_started(ejabberd_online_filter),
  ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, set_presence_hook, 50),
  ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, unset_presence_hook, 50),
  ejabberd_hooks:add(c2s_session_pending, Host, ?MODULE, c2s_session_pending, 50),
  ok.

stop(Host) ->
  ejabberd_hooks:delete(set_presence_hook, Host, ?MODULE, set_presence_hook, 50),
  ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, unset_presence_hook, 50),
  ejabberd_hooks:delete(c2s_session_pending, Host, ?MODULE, c2s_session_pending, 50),
  proc_lib:spawn(fun() ->
    ok = application:stop(ejabberd_online_filter)
  end),
  ok.

depends(_Host, _Opts) ->
  [].

mod_opt_type(_) ->
  [].

mod_options(_Host) -> [].

set_presence_hook(User, Server, Resource, Presence = #presence{ type = available }) ->
  ?INFO_MSG("set_presence_hook: User ~p, Server ~p, Resource ~p, Presence ~p", [User, Server, Resource, Presence]),
  Text = Presence#presence.status,
  ejabberd_online_filter_storage:put(Presence#presence.from, Presence#presence.show, #{ text => Text }),
  ok;

set_presence_hook(User, Server, Resource, Presence = #presence{ type = unavailable }) ->
  ?INFO_MSG("set_presence_hook(UNAVAILABLE): User ~p, Server ~p, Resource ~p, Presence ~p", [User, Server, Resource, Presence]),
  ejabberd_online_filter_storage:delete(Presence#presence.from),
  ok;

set_presence_hook(_User, _Server, _Resource, _ ) -> ok.

unset_presence_hook(User, Server, Resource, Status) ->
  Jid = jid:make(User, Server, Resource),
  ejabberd_online_filter_storage:delete(Jid),
  ?INFO_MSG("unset_presence_hook User ~p, Server ~p, Resource ~p, Status ~p ", [ User, Server, Resource, Status]),
  ok.

c2s_session_pending(State = #{ jid := Jid }) ->
  ejabberd_online_filter_storage:delete(Jid),
  ?INFO_MSG("c2s_session_pending: State ~p", [ State ]),
  State;

c2s_session_pending(State) -> State.

%%--------------------------------------------------------------------
%% ejabberd_http callback.
%%--------------------------------------------------------------------
-spec process([binary()], #request{}) -> {pos_integer(), [{binary(), binary()}], binary()}.
process(_LocalPath, _Request) ->
  try
    Jids = lists:map(fun({ User, Server }) ->
      jid:encode(jid:make(User, Server))
    end, ejabberd_online_filter_storage:get_jid_by_statuses([ undefined, chat ])),
    http_response(200, [{<<"Content-Type">>, <<"application/json">>}], jsx:encode(Jids))
  catch
    E:R:Stacktrace ->
      ?ERROR_MSG("Error ~p:~p ~p", [ E, R, Stacktrace]),
      http_response(500, [])
  end.

%% INTERNAL

-spec http_response(100..599) -> {pos_integer(), [{binary(), binary()}], binary()}.
http_response(Code) ->
  http_response(Code, []).

-spec http_response(100..599, [{binary(), binary()}]) -> {pos_integer(), [{binary(), binary()}], binary()}.
http_response(Code, ExtraHeaders) ->
  Message = <<(code_to_message(Code))/binary, $\n>>,
  http_response(Code, ExtraHeaders, Message).

-type http_body() :: binary() | {file, file:filename_all()}.
-spec http_response(100..599, [{binary(), binary()}], http_body()) -> {pos_integer(), [{binary(), binary()}], http_body()}.
http_response(Code, ExtraHeaders, Body) ->
  Headers = case proplists:is_defined(<<"Content-Type">>, ExtraHeaders) of
    true ->
      ExtraHeaders;
    false ->
      [{<<"Content-Type">>, <<"text/plain">>} | ExtraHeaders]
  end,
  {Code, Headers, Body}.

-spec code_to_message(100..599) -> binary().
code_to_message(201) -> <<"Upload successful.">>;
code_to_message(400) -> <<"Bad request">>;
code_to_message(403) -> <<"Forbidden.">>;
code_to_message(404) -> <<"Not found.">>;
code_to_message(405) -> <<"Method not allowed.">>;
code_to_message(413) -> <<"File size doesn't match requested size.">>;
code_to_message(500) -> <<"Internal server error.">>;
code_to_message(_Code) -> <<"">>.

encode_addr(IP) ->
  ejabberd_config:may_hide_data(misc:ip_to_list(IP)).

