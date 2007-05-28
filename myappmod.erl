-module(myappmod).
-author('klacke@bluetail.com').

-include("yaws_includes/yaws_api.hrl").
-compile(export_all).

box(Str) ->
    {'div',[{class,"box"}],
     {pre,[],Str}}.

out(A) ->
    {ehtml,
     [{p,[],
       box(io_lib:format("A#arg.appmoddata = ~p~n"
			 "A#arg.appmod_prepath = ~p~n"
			 "A#arg.querydata = ~p~n", 
			 [A#arg.appmoddata,
			  A#arg.appmod_prepath,
			  A#arg.querydata]))}]}.
			  
%out404(A, GC, SC) ->
%   {ehtml,
%    [{p,[],
%      box(io_lib:format("A#arg.appmoddata = ~p~n"
%                        "A#arg.appmod_prepath = ~p~n"
%                        "A#arg.querydata = ~p~n"
%                        "A#arg.req = ~p~n"
%                        "A#arg.clidata = ~p~n"
%                        "A#arg.pathinfo = ~p~n"
%                        "A#arg.headers = ~p~n",
%                        [A#arg.appmoddata,
%                         A#arg.appmod_prepath,
%                         A#arg.querydata,
%                         A#arg.req,
%                         A#arg.clidata,
%                         A#arg.pathinfo,
%                         A#arg.headers]))}]}.

out404(A, GC, SC) ->
  {ehtml, 
    [{p, [], 
      box(io_lib:format(
        "Arg Object: ~p~n"
        "GC Object: ~p~n"
        "SC Object: ~p~n"
        "Reformulation: ~p~n", [A, GC, SC, rails_forwarder:prepare_request(A,SC)]))
      }]}.
