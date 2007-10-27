-module(rails_forwarder).
-compile(export_all).
-include("yaws_api.hrl").
-include("yaws.hrl").

%-export([rails_create_responder/1, rails_create_responders/2, rails_handle_request/3, 
%         rails_handle_request_for/3, get_rails_response/2, prepare_request/1, decode_result/1]).

%create_responder(Command) ->
%  port_wrapper:wrap(Command).
%
%create_responders(Command, N) ->
%  create_responders_helper(Command, N, []).
%  
%create_responders_helper(_Command, 0, Results) ->
%  Results;
%create_responders_helper(Command, N, Results) ->
%  create_responders_helper(Command, N-1, [create_responder(Command)|Results]).

handle_request(Resource, Request, ServerOptions, Timeout) ->
  {_Node, Responder} = Resource,
  handle_request_for(Responder, Request, ServerOptions, self()),
  try get_rails_response(Responder, Timeout) of
    Any -> Any
  catch
    throw:timed_out -> 
      io:format("--- Timed out, removing resource and retrying!"),
      rails_connection_pool:remove(Resource),
      rails_forwarder:handle_request(rails_connection_pool:get(), Request, ServerOptions, Timeout)
  end.

handle_request_for(Responder, Request, ServerOptions, Target) ->
  Data = {request, prepare_request(Request, ServerOptions)},
  port_wrapper:send_from(Target, Responder, Data).
  
  
get_rails_response(Responder, Timeout) ->
  receive
    {Responder, Result} ->
      decode_result(Result);
    Any ->
      io:format("Unexpected message in get_rails_response: ~p~n", [Any])
  after Timeout ->
    throw(timed_out)
  end.
  
  
% Fugly data munging
prepare_request(Request, ServerOptions) ->
  Headers = Request#arg.headers,
  {convert_method(Request), convert_version(Request), convert_querypath(Request), 
   {querydata, prep(Request#arg.querydata)}, {servername, prep(ServerOptions#sconf.servername)},
   {headers, convert_headers(Request#arg.headers)},
   {cookies, list_to_tuple(lists:map(fun(X) -> prep(X) end, Headers#headers.cookie))},
   {pathinfo, prep(ServerOptions#sconf.docroot)},
   {postdata, Request#arg.clidata}}. % TODO: prepare request for railsification
  
decode_result({response, ResponseData}) ->
  convert_response(ResponseData).

convert_response(EhtmlTuple) ->
  {Status, AllHeaders, Html} = EhtmlTuple,
  {allheaders, HeaderList} = AllHeaders,
  ProcessedHeaderList = lists:map(fun({header, Name, Value}) -> {header, [binary_to_list(Name) ++ ":", binary_to_list(Value)]} end,
                                  tuple_to_list(HeaderList)),
  {html, RawResult} = Html,
  [Status, {allheaders, ProcessedHeaderList}, {html, binary_to_list(RawResult)}].

prep(A) when is_list(A) -> list_to_binary(A);
prep(A) -> A.

convert_method(Request) ->
  R = Request#arg.req,
  {http_request,Method,{_Type,_Path},_} = R,
  {method, Method}.

convert_querypath(Request)  ->
   R = Request#arg.req,
   {http_request,_Method,{_Type,Path},_} = R,
   {querypath, prep(Path)}.

convert_version(Request) ->
  R = Request#arg.req,
  {http_request,_Method,{_Type,_Path},Version} = R,
  {http_version, Version}.

convert_req(R) ->
  {http_request,Method,{_Type,Path},_} = R,
  {Method, prep(Path)}.

convert_headers(A) ->
  NormalHeaders = [{connection, prep(A#headers.connection)},
                   {accept, prep(A#headers.accept)},
                   {host, prep(A#headers.host)},
                   {if_modified_since, prep(A#headers.if_modified_since)},
                   {if_match, prep(A#headers.if_match)},
                   {if_none_match, prep(A#headers.if_none_match)},
                   {if_range, prep(A#headers.if_range)},
                   {if_unmodified_since, prep(A#headers.if_unmodified_since)},
                   {range, prep(A#headers.range)},
                   {referer, prep(A#headers.referer)},
                   {user_agent, prep(A#headers.user_agent)},
                   {accept_ranges, prep(A#headers.accept_ranges)},
                   {keep_alive, prep(A#headers.keep_alive)},
                   {location, prep(A#headers.location)},
                   {content_length, prep(A#headers.content_length)},
                   {content_type, prep(A#headers.content_type)},
                   {content_encoding, prep(A#headers.content_encoding)},
                   {authorization, prep(A#headers.authorization)},
                   {transfer_encoding, prep(A#headers.transfer_encoding)}],
   SpecialHeaders = lists:map(fun({http_header, _Len, Name, _, Value}) -> {prep(Name), prep(Value)} end, A#headers.other),
   list_to_tuple([{Name, Res} || {Name, Res} <- NormalHeaders, Res /= undefined] ++ SpecialHeaders).
