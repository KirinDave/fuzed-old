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
  register(?MODULE, spawn(fun() -> rails_connection_pool:loop([],[]) end)).

add(Rsrc) ->
  ?MODULE ! {add, Rsrc},
  ok.

remove(Rsrc) ->
  ?MODULE ! {remove, Rsrc},
  ok.

get() ->
  ?MODULE ! {get, self()},
  receive
    {node, X} -> 
      X
  end.

refund(Node) ->
  ?MODULE ! {refund, Node},
  ok.

list() ->
  ?MODULE ! {list, self()},
  receive
    {nodes, A} ->
      A
  end.

list_all() ->
  ?MODULE ! {list_all, self()},
  receive
    {all_nodes, A} ->
      A
  end.

remove_server(Server) ->
  ?MODULE ! {remove_server, Server},
  ok.

remove_all() ->
  ?MODULE ! {remove_all},
  ok.

remove_server_filter(Server, {Server, _X}) -> false;
remove_server_filter(_Server, {_NotServer, _X}) -> true.

loop([],A) ->
  receive
    {add, I} ->
      loop([I],[I|A]);
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
      end
  end;
loop(X,A) -> 
  receive 
    {add, I} ->
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
    Other ->
      io:format("~p loop(X,A) Received unknown message: ~p~n", [?MODULE, Other]),
      loop(X,A)
  end.
