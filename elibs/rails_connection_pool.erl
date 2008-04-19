-module(rails_connection_pool). 
-compile(export_all).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-behaviour(gen_server).


%% Convenience Function
simple_handle_request(Arg,ServerInfo) ->
  {Source, Resource} = rails_connection_pool:get(),
  Response = rails_forwarder:handle_request(Resource, Arg, ServerInfo, 10000),
  rails_connection_pool:refund({Source,Resource}),
  Response.
  

handle_request(Arg,ServerInfo) ->
  handle_request_helper(Arg,ServerInfo,0).

handle_request_helper(_Arg,_ServerInfo,Retries) when Retries > 1 ->
  throw(timed_out);
handle_request_helper(Arg,ServerInfo,_Retries) ->
  Remote = fun(A,S,Target) ->
    {Source, Handler} = rails_connection_pool:get(),
    Response = rails_forwarder:rails_handle_request(Handler, A, S, 10000),
    Target ! {rails_response, Response},
    rails_connection_pool:refund({Source, Handler}) end,
  _RequestProc = spawn(Remote(Arg,ServerInfo,self())),
  receive
    {rails_response, Response} ->
      Response
%  after 5000 ->
%    handle_request_helper(Arg,ServerInfo, Retries+1)
  end.
  

% State Record

-record(state, 
  {
   active_nodes = [],
   nodes = [],
   pending_requests = queue:new(),
   details = []
  }
).

%
%% API
%

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [[]], []).

start() ->
  gen_server:start({global, ?MODULE}, ?MODULE, [[]], []).

add(Rsrc) ->
  gen_server:cast({global, ?MODULE}, {add, Rsrc}).

remove(Rsrc) ->
  gen_server:cast({global, ?MODULE}, {remove, Rsrc}).

get() ->
  gen_server:cast({global, ?MODULE}, {get, self()}),
  receive 
    {resource, Rsrc} -> Rsrc
  end.
  
cycle_all(Delay) ->
  gen_server:cast({global, ?MODULE}, {cycle_all, Delay}).



refund(Node) ->
  gen_server:call({global, ?MODULE}, {refund, Node}).

list() ->
  gen_server:call({global, ?MODULE}, {list}).

list_all() ->
  gen_server:call({global, ?MODULE}, {list_all}).
  
pending_size() ->
  gen_server:call({global, ?MODULE}, {pending_size}).

identity() ->
  gen_server:call({global, ?MODULE}, {identity}).

%
%% gen_server callbacks
%

init([Details]) ->
  io:format("~p starting~n", [?MODULE]),
  {ok, #state{details=Details}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({list}, _Source, State) ->
  {reply, State#state.active_nodes, State} ;
handle_call({list_all}, _Source, State) -> 
  {reply, State#state.nodes, State} ;
handle_call({pending_size}, _Source, State) -> 
  {reply, queue:len(State#state.pending_requests), State} ;
handle_call({flush_pending}, _Source, State) -> 
  {reply, length(State#state.pending_requests), State#state{pending_requests=queue:new()}} ;
handle_call({details}, _Source, State) -> 
  {reply, State#state.details, State} ;
handle_call({refund, Resource}, _Source, State) -> 
  % Only work if this node is actually a member of the pool
  IsMember = lists:member(Resource, State#state.nodes),
  if IsMember -> 
    QueueIsEmpty = queue:is_empty(State#state.pending_requests),
    if QueueIsEmpty ->
         {reply, ok, State#state{active_nodes=[Resource|State#state.active_nodes]}} ;
       true -> 
         {{value, Waiting}, NewQueue} = queue:out(State#state.pending_requests),
         Waiting ! {resource, Resource},
         {reply, ok, State#state{pending_requests=NewQueue}}
    end ;
    true -> 
    {reply, not_a_member, State}
  end;
handle_call({identity}, _Source, State) ->
  {reply, self(), State}.




%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({remove, Rsrc}, State) -> 
  {noreply,  State#state{ nodes=lists:delete(Rsrc, State#state.nodes),
                          active_nodes=lists:delete(Rsrc, State#state.active_nodes)}};
handle_cast({add, Rsrc}, State) -> 
  {Nodes, ActiveNodes} = {State#state.nodes, State#state.active_nodes},
  ToAdd = not lists:member(Rsrc, Nodes),
  if 
    ToAdd ->
      error_logger:info_msg("Adding resource: ~p~n", [Rsrc]),
      {_RNode, RPid} = Rsrc,
      pool_sweeper:watch(RPid),
      {noreply, State#state{nodes=[Rsrc|Nodes], active_nodes=[Rsrc|ActiveNodes]}};
    true ->
      {noreply, State}
  end ;
handle_cast({get, For}, State) -> 
  if 
    length(State#state.active_nodes) > 0 -> 
      [Head|Rest] = State#state.active_nodes,
      For ! {resource, Head},
      {noreply, State#state{active_nodes=Rest}} ;
    true ->
      Q = queue:in(For, State#state.pending_requests),
      {noreply, State#state{pending_requests=Q}}
  end;
handle_cast({cycle_all, Delay}, State) ->
  Tuples = State#state.nodes,
  Fun = fun({X, _Pid}, Acc) ->
    Member = lists:member(X, Acc),
    if
      Member ->
        Acc;
      true ->
        [X|Acc]
    end
  end,
  Nodes = lists:foldl(Fun, [], Tuples),
  error_logger:info_msg("Cycling all nodes: ~p~n", [Nodes]),
  lists:foreach(fun(Node) -> resource_manager:cycle(Node), timer:sleep(Delay * 1000) end, Nodes),
  {noreply, State}.


handle_info(Any,S) -> 
  io:format("Got INFO ~p~n", [Any]),
  {noreply, S}.

%%--------------------------------------------------------------------
%% Func: length(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


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
%% Internal API calls
%%--------------------------------------------------------------------
