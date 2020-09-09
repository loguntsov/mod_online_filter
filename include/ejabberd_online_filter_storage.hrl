-author("begemot").

-record(user_status, {
  jid :: ljid(),
  index :: { User :: binary(), Server :: binary() },
  status :: binary(),
  updated_at :: erlang:timestamp(),
  other = #{} :: maps:map()
}).
