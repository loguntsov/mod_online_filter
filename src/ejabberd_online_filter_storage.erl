-module(ejabberd_online_filter_storage).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
  init/0,
  put/2, put/3,
  get/1, get/2,
  delete/1,
  get_by_status/1, get_jid_by_statuses/1
]).

-include("xmpp.hrl").
-include("ejabberd_online_filter_storage.hrl").

-define(TABLE, user_status).

init() ->
  ejabberd_mnesia:create(?MODULE, ?TABLE,
    [{ram_copies, [node()]},
      {attributes, record_info(fields, user_status)}]),
  mnesia:add_table_index(?TABLE, key),
  mnesia:add_table_index(?TABLE, status),
  ok.

put(Jid, Status) ->
  put(Jid, Status, #{}).

put(Jid = #jid{}, Status, Other) when is_map(Other) ->
  LJid = { User, Server, _ } = jid:tolower(Jid),
  ok = mnesia:dirty_write(?TABLE, #user_status{
    jid = LJid,
    index = { User, Server },
    status = Status,
    updated_at = os:timestamp(),
    other = Other
  }),
  ok.

delete(Jid = #jid{}) ->
  LJid = jid:tolower(Jid),
  mnesia:dirty_delete(?TABLE, LJid).

get(User, Server) ->
  Key = { User, Server },
  mnesia:dirty_index_read(?TABLE, index, Key).

get(Jid = #jid{}) ->
  { User, Server, _ } = jid:tolower(Jid),
  get(User, Server).

get_by_status(Status) ->
  mnesia:dirty_index_read(?TABLE, Status, status).

get_jid_by_statuses(Statuses) ->
  List = lists:flatmap(fun(Status) ->
    Records = get_by_status(Status),
    [ St#user_status.index || St <- Records ]
  end, Statuses),
  lists:usort(List).

