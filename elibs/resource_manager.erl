%%%-------------------------------------------------------------------
%%% File    : /Users/dfayram/Projects/concilium/elibs/resource_manager.erl
%%% Author  : David Fayram
%%%-------------------------------------------------------------------
-module(resource_manager).
-behaviour(gen_server).

%% API exports
-export([start_link/3, start/3,nodes/0,nodecount/0,change_nodecount/1,cycle/0]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Erlang records are ugly.
-record(state, {generator = fun() -> undefined end,
                terminator = fun(_) -> undefined end,
                nodecount = 1,
                nodes = [],
                term_hook = fun(_) -> undefined end
               }).

%% External call functions

% Note the local server, one of these should run on every
% node serving up rails responders.
start_link(Generator, Terminator, NumNodes) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Generator, Terminator, NumNodes], []).
start(Generator, Terminator, NumNodes) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Generator, Terminator, NumNodes], []).

nodes() -> gen_server:call(?MODULE, nodes).
nodecount() -> gen_server:call(?MODULE, nodecount).
change_nodecount(NewNodecount) -> gen_server:cast(?MODULE, {change_nodecount, NewNodecount}).
cycle() -> gen_server:cast(?MODULE, cycle).

%% GEN_SERVER callbacks.
init([Generator, Terminator, NumNodes]) ->
    process_flag(trap_exit, true),
    Nodes = spawn_nodes(Generator, NumNodes),
    {ok, #state{generator = Generator, nodecount = NumNodes, 
                nodes = Nodes, terminator = Terminator}}.

handle_call(term_hook, _From, State) ->
  {reply, State#state.term_hook, State};
handle_call({term_hook, Hook}, _From, State) when is_function(Hook, 1) ->
  {reply, State#state.term_hook, State#state{term_hook = Hook}};
handle_call(nodecount,_From,State) -> 
  {reply, State#state.nodecount, State};
handle_call(nodes, _From, State) ->
  {reply, State#state.nodes, State}.

handle_cast(cycle, State) -> 
  drop_nodes(State#state.terminator, State#state.nodes),
  {noreply, State#state{nodes=spawn_nodes(State#state.generator, State#state.nodecount)}};
handle_cast({change_nodecount, NewCount}, S) when is_number(NewCount) -> 
  Count = S#state.nodecount,
  if
    NewCount > Count ->
      {noreply, 
        S#state{nodecount = NewCount, 
                nodes = spawn_nodes(S#state.generator, NewCount - Count) ++ S#state.nodes}};
    NewCount < Count ->
      {ToKill, ToKeep} = lists:split(NewCount - Count, S#state.nodes),
      drop_nodes(S#state.terminator, ToKill),
      {noreply, S#state{nodecount=NewCount, nodes=ToKeep}};
    true -> 
      {noreply, S}
  end.

handle_info({'EXIT', Pid, _Reason}, S) -> 
  Term = S#state.terminator,
  Membership = lists:any(fun(X) -> X =:= Pid end, S#state.nodes), 
  if
    Membership -> 
      Term(Pid),
      Res = lists:delete(Pid, S#state.nodes),
      NewNode = spawn_linked_node(S#state.generator),
      {noreply, S#state{nodes=[NewNode|Res]}};
    true -> 
      {noreply, S}
  end;
handle_info(Any,S) -> 
  io:format("Got INFO ~p~n", [Any]),
  {noreply, S}.

terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Utility functions

spawn_linked_node(Generator) -> 
  Node = Generator(),
  link(Node),
  Node.

spawn_nodes(Generator,NumNodes) ->
  spawn_nodes(Generator,NumNodes,[]).

spawn_nodes(_Generator,0,Acc) -> Acc;
spawn_nodes(Generator,NumNodes,Acc) -> spawn_nodes(Generator,NumNodes - 1, [spawn_linked_node(Generator)|Acc]).

drop_nodes(Terminator, Nodes) -> 
  Killer = fun(Node) -> unlink(Node), Terminator(Node) end,
  lists:map(Killer, Nodes).