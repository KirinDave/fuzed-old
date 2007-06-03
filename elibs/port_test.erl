-module(port_test).
-compile(export_all).



provide_responder(Responder) ->
  rails_connection_pool:add({node(), Responder}).

start() ->
  Commands = lists:duplicate(4, "../helloworld/script/rack.rb"),
  go(Commands).

go(Commands) ->
  Ports = lists:map(fun(X) -> port_wrapper:wrap(X) end, Commands),
  lists:map(fun(X) -> provide_responder(X) end, Ports).
