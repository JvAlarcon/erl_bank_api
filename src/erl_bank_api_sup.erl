% This is a supervisor file with behaviour of the same name
% This behaviour is a core OTP mechanism, and it's purpouse is to monitor, start and restart worker processes or other supervisors
-module(erl_bank_api_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(POOL_NAME, db_pool).
-define(POOL_SIZE, 10).
-define(POOL_MAX_OVERFLOW, 50).

% The purpose here is to start and register the supervisor
% DbConfig comes from erl_bank_api_app:db_config_from_env().
-spec start_link(map()) -> supervisor:startlink_ret().
start_link(DbConfig) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, DbConfig).

% OTP supervisor callback - Here we declare all childs specs
init(DbConfig) ->
    % Child 1 - Elli HTTP server
    ElliOpts = [{callback, bank_callback}, {port, 3000}],
    ElliSpec = {
        bank_http,
        {elli, start_link, [ElliOpts]},
        permanent,
        5000,
        worker,
        [elli]
    },
    
    % Child 2 - Poolboy pool of connections
    PoolSpec = poolboy:child_spec(
		?POOL_NAME,
		[
		 {name, {local, ?POOL_NAME}},
		 {worker_module, db_worker},
		 {size, ?POOL_SIZE},
		 {max_overflow, ?POOL_MAX_OVERFLOW}
		],
		DbConfig
    ),

    % The first map after ok atom is the Supervisor flags
    % With one_for_one a crash in Elli doesn't restart the DB Pool and vice-versa
    % The second and third argument are intensity and period respectively
    % It stands for: At most 5 restarts in 10 seconds before giving up.
    % So the supervisor will try to restart 5 times in 10 seconds in case of failure, and will give up after that tries and time
    {ok, {{one_for_one, 5, 10}, [ElliSpec, PoolSpec]}}.
