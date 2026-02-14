-module(db_operations).

-export([query/1,
	 query/2,
	 transaction/1]).

-define(POOL, db_pool).
-define(TIMEOUT, 3000).

% The behaviour hereof the functions below is the following:
% Check a worker out of the pool;
% Executes the query;
% Then return the worker immediately.

% Run a plain SQL without parameters
-spec query(iodata()) ->
    {ok, [epgsql:column()], [tuple()]} | {error, term()}.
query(Sql) ->
    poolboy:transaction(?POOL,
	fun(Worker) ->
	    gen_server:call(Worker, {query, Sql}, ?TIMEOUT)
	end).

% Run a plain SQL with parameters
-spec query(iodata(), [term()]) ->
    {ok, [epgsql:column()], [tuple()]} | {error, term()}.
query(Sql, Params) ->
    poolboy:transaction(?POOL,
        fun(Worker) ->
	    gen_server:call(Worker, {query, Sql, Params}, ?TIMEOUT)
        end).

% Run a zero argument fun/1 inside a Postgres transaction
% The fun recieves the epgsql connection pid directly so it can execute multiple statements atomically
% Useful to run differents queries inside a single transaction
-spec transaction(fun((pid()) -> any())) -> any() | {error, term()}.
transaction(Fun) ->
    poolboy:transaction(?POOL,
        fun(Worker) ->
	    % First, ask the worker for it's connection pid via a dedicated call
	    Conn = gen_server:call(Worker, get_conn, ?TIMEOUT),
	    % Then execute the queries inside a transaction
	    epgsql:with_trasaction(Conn, Fun)
	end).
