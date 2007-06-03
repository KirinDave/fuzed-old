-module(rails_connection_pool). 
-compile(export_all).


%% Convenience Function
simple_handle_request(Arg,ServerInfo) ->
  {Source, Resource} = rails_connection_pool:get(),
  Response = rails_forwarder:rails_handle_request(Resource, Arg, ServerInfo, 10000),
  rails_connection_pool:refund({Source,Resource}),
  Response.
  

handle_request(Arg,ServerInfo) ->
  handle_request_helper(Arg,ServerInfo,0).

handle_request_helper(_Arg,_ServerInfo,Retries) when Retries > 1 ->
  throw(timed_out);
handle_request_helper(Arg,ServerInfo,Retries) ->
  Remote = fun(A,S,Target) ->
    {Source, Handler} = rails_connection_pool:get(),
    Response = rails_forwarder:rails_handle_request(Handler, A, S, 10000),
    Target ! {rails_response, Response},
    rails_connection_pool:refund({Source, Handler}) end,
  _RequestProc = spawn(Remote(Arg,ServerInfo,self())),
  receive
    {rails_response, Response} ->
      Response
  after 5000 ->
    handle_request_helper(Arg,ServerInfo, Retries+1)
  end.
  

%% Server manipulation
start() -> 
  global:register_name(?MODULE, spawn(
    fun() -> 
      process_flag(trap_exit, true),
      rails_connection_pool:loop([],[]) end
    )
  ).

add({Node, Proc}) when is_pid(Proc) ->
  global:send(?MODULE, {add, {Node, Proc}}),
  ok.

remove(Rsrc) ->
  global:send(?MODULE, {remove, Rsrc}),
  ok.

get() ->
  global:send(?MODULE, {get, self()}),
  receive
    {node, X} -> 
      X
  end.

refund(Node) ->
  global:send(?MODULE, {refund, Node}),
  ok.

list() ->
  global:send(?MODULE, {list, self()}),
  receive
    {nodes, A} ->
      A
  end.

list_all() ->
  global:send(?MODULE, {list_all, self()}),
  receive
    {all_nodes, A} ->
      A
  end.

remove_server(Server) ->
  global:send(?MODULE, {remove_server, Server}),
  ok.

remove_all() ->
  global:send(?MODULE, {remove_all}),
  ok.

remove_server_filter(Server, {Server, _X}) -> false;
remove_server_filter(_Server, {_NotServer, _X}) -> true.

loop([],A) ->
  receive
    {add, {Node, Proc}} when is_pid(Proc) ->
      erlang:link(Proc),
      loop([{Node,Proc}],[{Node,Proc}|A]);
    {list, Pid} ->
      Pid ! {nodes, []},
      loop([],A);
    {list_all, Pid} ->
      Pid ! {all_nodes, A},
      loop([],A);
    {refund,Node} ->
      Membership = lists:member(Node,A),
      if 
        Membership ->
          loop([Node],A);
        true ->
          loop([],A)
      end;
    {'EXIT', Pid, _Reason} -> 
      FilterFun = fun({_Node,MPid}) -> MPid /= Pid end,
      loop([],lists:filter(FilterFun, A))
  end;
loop(X,A) -> 
  receive 
    {add, {Node, Proc}} when is_pid(Proc)->
      erlang:link(Proc),
      I = {Node, Proc},
      loop([I|X], [I|A]);
    {remove, I} -> 
      loop(lists:delete(I,X), lists:delete(I,A));
    {remove_server, Server} ->
      PurgedX = lists:filter(fun(Z) -> remove_server_filter(Server, Z) end, X),
      PurgedA = lists:filter(fun(Z) -> remove_server_filter(Server, Z) end, A),
      loop(PurgedX, PurgedA);
    {remove_all} ->
      loop([],[]);
    {list, Pid} ->
      Pid ! {nodes, X},
      loop(X,A);
    {list_all, Pid} ->
      Pid ! {all_nodes, A},
      loop(X,A);
    {get, Pid} ->
      [Node|Rest] = X,
      Pid ! {node, Node},
      loop(Rest,A);
    {refund,Node} ->
      Membership = lists:member(Node,A),
      if 
        Membership ->
          loop(X ++ [Node],A);
        true ->
          loop(X,A)
      end;
    {'EXIT', Pid, _Reason} -> 
      FilterFun = fun({_Node,MPid}) -> MPid /= Pid end,
      loop(lists:filter(FilterFun,X),lists:filter(FilterFun, A));
    Other ->
      io:format("~p loop(X,A) Received unknown message: ~p~n", [?MODULE, Other]),
      loop(X,A)
  end.
