-module(praytotype).
-compile(export_all).

test() ->
  Port = open_port({spawn, "ruby ./pray.rb"}, [{packet, 4}, use_stdio, exit_status, binary]),
  Data = term_to_binary({hello, <<"hello world">>}),
  Data2 = term_to_binary({whee, <<"Good times">>}),
  Port ! {self(), {command, Data}},
  Port ! {self(), {command, Data2}}.