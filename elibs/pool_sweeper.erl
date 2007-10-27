%%%-------------------------------------------------------------------
%%% Author  : Tom Preston-Werner
%%%-------------------------------------------------------------------
-module(pool_sweeper).
-behaviour(gen_server).

%% API
-export([start_link/0, watch/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-record(state, 
  {
    node_pids = dict:new()
  }
).

%%====================================================================
%% API
%%====================================================================

watch(Pid) -> 
  gen_server:cast(?MODULE, {watch, Pid}).
  
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  io:format("~p starting~n", [?MODULE]),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({watch, Pid}, State) ->
  Node = node(Pid),
  error_logger:info_msg("Now monitoring node ~p~n", [Node]),
  #state{node_pids=NodePids} = State,
  
  case dict:find(Node, NodePids) of
    {ok, _Pids} ->
      NodePidsNew = dict:append(Node, Pid, NodePids);
    error ->
      NodePidsNew = dict:store(Node, [Pid], NodePids),
      monitor_node(Node, true)
  end,
  
  {noreply, State#state{node_pids=NodePidsNew}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({nodedown, Node}, State) ->
  error_logger:warning_msg("Node ~p went away. Removing from pools.~n", [Node]),
  #state{node_pids=NodePids} = State,
  Pids = dict:fetch(Node, NodePids),
  
  remove_pids_from_pool(Pids),
  
  NodePidsNew = dict:erase(Node, NodePids),
  
  {noreply, State#state{node_pids=NodePidsNew}}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

remove_pids_from_pool([]) ->
  ok;
remove_pids_from_pool([Pid|Rest]) ->
  rails_connection_pool:remove({node(Pid), Pid}),
  remove_pids_from_pool(Rest).
