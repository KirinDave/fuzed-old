-module(join_cluster).
-compile(export_all).

start(Server, Command) ->
  code:add_patha("./elibs"),
  erlang:set_cookie(node(), 'AWZVAQXUPGVPKZQLYEUP'),
  net_adm:ping(Server), sleep_in_HEAVENLY_PEACE(2),
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