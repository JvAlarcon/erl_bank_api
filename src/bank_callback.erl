-module(bank_callback).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, Args) ->
    handle(Req#req.method, elli_request:path(Req), Req, Args).

handle('POST',[<<"">>], Req, Args) ->
    
