% This file is a poolboy worker that owns a single epgsql connection.
-module(db_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

% This export is the public API
-export([start_link/1]).

% This one is for the gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/2]).

-record(state, {
    conn :: pid()
}).

% Called by Poolboy to start a worker
-spec start_link(map()) -> gen_server:start_ret().
start_link(DbConfig) ->
    gen_server:start_link(?MODULE, DbConfig, []).

init(#{host := Host, port := Port, database := Db,
       username := User, password := Password}) ->
    case epgsql:connect(#{host => Host,
			  port => Port,
			  dabatase => Db,
			  username => Username,
			  password => Password,
			  timeout => 3000}) of
	{ok, Conn} -> {ok, #state{conn = Conn}};
	{error, Reason} -> {stop, Reason}
    end.

% We are using pattern matching again to have different behaviours for handle call
% Erlang can be clean solely by pattern matching
% Return the raw epgsql connection pid
handle_call(get_conn, _From, #state{conn = Conn} = State) ->
    {reply, Conn, State};
% Synchronous query without parameters
% Returns {ok, Cols, Rows} | {error, Reason}
handle_call({query, Sql}, _From, #state{conn = Conn} = State) ->
    Result = epgsql:squery(Conn, Sql),
    {reply, Result, State};
% Parameterised query
% Returns {ok, Cols, Rows} | {error, Reason}
handle_call({query, Sql, Params}, _From, #state{conn = Conn} = State) ->
    Result = epgsql:squery(Conn, Sql, Params),
    {reply, Result, State};
% Default Match
handle_call(_Request, _From, State) ->
    {reply, {error, unknow_request}, State}.

% The functions belows are mandatory OTP callbacks required by gen_server behaviour

% This is used to handle assynchronous messages
% Right now this will not be used
handle_cast(_Msg, State) ->
    {noreply, State}.

% Will handle any message in the process mailbox that was not send by gen_server:call or gen_server:cast
% Right now, we are letting any error propagate to the supervisor
handle_info(_Info, State) ->
    {noreply, State}.

% This is called when the server is about to be shutdown
% We need to make sure the connection will be closed before the shutdown
terminate(_Reason, #state{conn = Conn}) ->
    epgsql:close(Conn),
    ok.

% This will only be called by a hot code upgrade, i.e, updating a running module with a new version without stopping the node
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
