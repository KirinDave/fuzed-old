%%%-------------------------------------------------------------------
%%% Author  : Tom Preston-Werner
%%%-------------------------------------------------------------------
-module(fuzed_sup).
-behaviour(supervisor).

-export([start/0, start_shell/0, start_link/1, init/1]).

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
  {ok, {{one_for_one, 3, 10},
    [{rails_connection_pool,
       {rails_connection_pool, start_link, []},
       permanent,
       10000,
       worker,
       [rails_connection_pool]},
     {pool_sweeper,
       {pool_sweeper, start_link, []},
       permanent,
       10000,
       worker,
       [pool_sweeper]}
    ]}}.