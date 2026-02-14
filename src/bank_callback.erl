-module(bank_callback).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

% Handle works by pattern matching - There is no rout config file
% Take note that the last handle is essential to be the default case
handle('POST', [<<"transactions">>, <<"deposit">>], Req) ->
    Body = elli_request:body(Body),
    {<<"id_account">> := IdAccount, <<"amount">> := Amount} = jsx:decode(Body, [return_maps]),
    % Insert in database
    {ok, [], Response};
handle('POST', [<<"transactions">>, <<"withdrawal">>], Req) ->
    Body = elli_request:body(Body),
    {<<"id_account">> := IdAccount, <<"amount">> := Amount} = jsx:decode(Body, [return_maps]),
    % Insert in database
    {ok, [], Response};
handle('POST', [<<"transactions">>,<<"transfers">>], Req) ->
    Body = elli_request:body(Body),
    {<<"id_account_from">> := IdAccountFrom,
     <<"id_account_to">> := IdAccountTo,
     <<"amount">> := Amount} = jsx:decode(Body, [return_maps]),
    % Insert in database
    {ok, [], Response};
handle('GET', [<<"accounts">>, <<"details">>, Id]) ->
    {};
handle('GET', [<<"accounts">>, <<"statement">>], Id) ->
    {};
handle(_,_,_Req) ->
    {404, [], <<"Not found">>}.

% With handle_event/3 you can get every event in handle/2 from elli
% This first one is to log every successful event
handle_event(request_complete, [Req, ResponseCode, _Headers, _Body, Timings], _Args) ->
    io:format("~p ~p completed with ~p~n",
	      [Req#req.method, elli_request:path(Req), ResponseCode]),
    ok;
% This second one is to log every time the handle crashes
handle_event(request_throw, [Req, Exception, Stacktrace], _Args) ->
    io:format("Handler crashed: ~p~n~p~n", [Exception, Stacktrace]),
    ok;
% This third one is to log every timeout that occurs
handle_event(request_timeout, [Req], _Args) ->
    io:format("Timeout on ~p~n", [elli_request:path(Req)]),
    ok;
% This is a catch-all for events that we don't care or will not handle
% This is required even if we don't have any of the events above
% Again, everything is pattern matching in Elli
handle_event(_Event, _Data, _Args) -> ok.
