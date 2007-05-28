-module(join_cluster).
-compile(export_all).

main(_X) ->
  code:add_patha("./elibs"),
  erlang:set_cookie(node(), 'AWZVAQXUPGVPKZQLYEUP'),
  io:format("Adding 4 responders to the demo cluster: chisai.local.~n"),
  Command = "./helloworld/script/rack.rb",
  lists:map(fun(Cmd) -> create_and_add(Cmd) end, lists:duplicate(4, Command)).

create_and_add(Cmd) ->
  port_test:provide_responder(server@chisai.local, port_wrapper:wrap(Cmd)).