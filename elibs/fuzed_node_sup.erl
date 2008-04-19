%%%-------------------------------------------------------------------
%%% Author  : Tom Preston-Werner
%%%-------------------------------------------------------------------
-module(fuzed_node_sup).
-behaviour(supervisor).
-export([start/0, start_shell/0, start_link/1, init/1]).


% Supervisor Functions

start() ->
  spawn(fun() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = [])
  end).
  
start_shell() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
  unlink(Pid).
  
start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).
  

init([]) ->
  ping_master(application:get_env(master)),
  init_helper(application:get_env(command), application:get_env(num_nodes)).
  

% Helper functions

sleep(Time) ->
  receive
  after Time ->
    ok
  end.

ping_master({ok, Node}) -> pong = net_adm:ping(Node), sleep(5000);
ping_master(_) -> noop.

init_helper({ok, Command}, {ok, NumNodes}) when integer(NumNodes) ->
  error_logger:info_msg("Starting with command: ~p~n", [Command]),
  error_logger:info_msg("Starting with ~p nodes~n", [NumNodes]),
  start_rm(Command, NumNodes);
init_helper({ok, Command}, undefined) ->
  error_logger:info_msg("Starting with command: ~p~n", [Command]),
  error_logger:info_msg("Starting with default (1) nodes~n"),
  start_rm(Command, 1).

start_rm(Command, NumNodes) -> 
  Initalizer = fun(Responder) -> 
                   rails_connection_pool:add({node(), Responder}),
                   {ok, Responder}
               end,
  Destructor = fun(Responder) -> 
                   rails_connection_pool:remove({node(), Responder}),
                   ok
               end,
  {ok, {{one_for_one, 3, 10},
    [{resource_manager,
      {resource_manager, start_link, [node(), {Command, NumNodes},
                                      Initalizer, Destructor, 60000]},
      permanent,
      10000,
      worker,
      [resource_manager]}
    ]}}.
