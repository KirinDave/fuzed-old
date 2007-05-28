-module(port_test).
-compile(export_all).

local_provide_responder(Responder) ->
  rails_connection_pool:add({node(), Responder}).

provide_responder(Node, Responder) ->
  rpc:call(Node, rails_connection_pool, add, [{node(),Responder}]).

start() ->
  Commands = lists:duplicate(4, "../helloworld/script/rack.rb"),
  go(Commands).

start_local() ->
  Commands = lists:duplicate(4, "./helloworld/script/rack.rb"),
  go_local(Commands).

go(Commands) ->
  Ports = lists:map(fun(X) -> port_wrapper:wrap(X) end, Commands),
  lists:map(fun(X) -> provide_responder(server@localhost, X) end, Ports).

go_local(Commands) ->
  Ports = lists:map(fun(X) -> port_wrapper:wrap(X) end, Commands),
  lists:map(fun(X) -> local_provide_responder(X) end, Ports).
