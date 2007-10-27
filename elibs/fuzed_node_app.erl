-module(fuzed_node_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
  fuzed_node_sup:start_link(StartArgs).
  
stop(_State) ->
  ok.