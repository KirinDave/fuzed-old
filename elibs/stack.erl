-module(stack).

-export([start/0]).

start() ->
  application:load(fuzed),
  application:start(fuzed),
  application:load(fuzed_node),
  application:start(fuzed_node).