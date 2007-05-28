-module(port_wrapper).
-compile(export_all).

wrap(Command) ->
  spawn(fun() -> Port = create_port(Command), loop(Port) end).

create_port(Command) ->
  open_port({spawn, Command}, [{packet, 4}, use_stdio, exit_status, binary]).

loop(Port) ->
  receive
    shutdown ->
      port_close(Port);
    {Source, {command, Message}} -> 
      Port ! {self(), {command, Message}},
      receive
        {Port, Result} ->
          Source ! {self(), Result}
      end,
      loop(Port)
  end.
    
  
