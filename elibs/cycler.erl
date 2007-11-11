-module(cycler).

-export([cycle/1, cycle_all/1, cycle_all/2]).

cycle([Node]) when is_list(Node) ->
  cycle(list_to_atom(Node));
cycle(Node) ->
  pong = net_adm:ping(Node),
  timer:sleep(2000),
  resource_manager:cycle(Node),
  ok.
  
cycle_all([Master, Delay]) ->
  cycle_all(list_to_atom(Master), list_to_integer(Delay)),
  ok.
  
cycle_all(Master, Delay) ->
  pong = net_adm:ping(Master),
  timer:sleep(2000),
  rails_connection_pool:cycle_all(Delay),
  ok.