-module(join_cluster).
-compile(export_all).

main(_X) ->
  code:add_patha("./elibs"),
  erlang:set_cookie(node(), 'AWZVAQXUPGVPKZQLYEUP'),
  io:format("Adding 4 responders to the demo cluster: chisai.local.~n"),
  Command = "./helloworld/script/rack.rb",
  net_adm:ping(server@localhost), sleep_in_HEAVENLY_PEACE(2),
  start_resource_manager(Command, 4).

start_resource_manager(Command, N) -> 
  Spawner = fun() -> 
    Responder = port_wrapper:wrap(Command),
    rails_connection_pool:add({node(), Responder}),
    Responder
  end,
  Slayer = fun(Responder) -> 
    Responder ! shutdown,
    rails_connection_pool:remove({node(), Responder})
  end,
  resource_manager:start_link(Spawner,Slayer,N).

sleep_in_HEAVENLY_PEACE(Secs) -> 
  receive
  after Secs * 1000 -> 
    Secs * 1000
  end.