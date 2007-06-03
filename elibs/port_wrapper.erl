-module(port_wrapper).
-export([wrap/1, send/2, send_from/3, shutdown/1, rpc/2]).
-author('Dave Fayram').

wrap(Command) ->
 spawn(fun() -> process_flag(trap_exit, true), Port = create_port(Command), link(Port), loop(Port, [
]) end).
  
rpc(WrappedPort, Message) -> 
  send(WrappedPort, Message),
  receive
    {WrappedPort, Result} -> Result
  after 10000 -> 
    {WrappedPort, timed_out}
  end.
  
send(WrappedPort, Message) ->
  WrappedPort ! {self(), {command, term_to_binary(Message)}},
  WrappedPort.

send_from(Target, WrappedPort, Message) -> 
  io:format("(S:~p -> R~p) ~p~n: ~p", [self(), WrappedPort, Message,
                                      {Target, {command, term_to_binary(Message)}} ]),
  WrappedPort ! {Target, {command, term_to_binary(Message)}},
  WrappedPort.

  
shutdown(WrappedPort) -> 
  WrappedPort ! shutdown,
  true.

create_port(Command) ->
  open_port({spawn, Command}, [{packet, 4}, nouse_stdio, exit_status, binary]).

loop(Port, Monitors) ->
  receive
    shutdown ->
      port_close(Port),
      exit(shutdown);
    {Source, {command, Message}} -> 
      Port ! {self(), {command, Message}},
      receive
        {Port, {data, Result}} ->
          Source ! {self(), binary_to_term(Result)}
      end,
      loop(Port,Monitors);
      
    {monitor, Proc} when is_pid(Proc) ->
      loop(Port, [Proc|Monitors]);
      
    {Port, {exit_status, _Code}} ->
      %port_close(Port)
      exit(external_failure);
    Any -> 
      io:format("----------~nWrapper got: ~p~n----------~n", [Any]),
      loop(Port, Monitors)
  end.