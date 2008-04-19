-module(rails_handler).
-compile(export_all).


out404(A, _GC, SC) ->
  Resource = rails_connection_pool:get(),
  Result = rails_forwarder:handle_request(Resource, A, SC, 60000),
  rails_connection_pool:refund(Resource),
  Result.
