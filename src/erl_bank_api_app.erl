-module(erl_bank_api_app).
-behaviour(application).

% This is necessary for the file with application behaviour works
-export([start/2, stop/1]).

% Return the value of an OS variable
% If variable is not present or is empty, crashes the application
-spec required_env() -> string().
require_env(Name) ->
    case os:getenv(Name) of
	false ->
	    error({missing_required_env_variable},
		  Name,
		  "Set this variable before starting the application.");
	"" ->
	    error({empty_required_env_variable},
		  Name,
		  "Variable is present but his value is empty.");
	Value -> Value
    end.

% Read the db connection parameters from OS environment variables
% DBHOST and DBPORT are optional, if not present, a default value will be used
% DATABASE, DBUSER and DBPASSWORD are required - If they are not present, the application will crash
-spec db_config_from_env() -> map().
db_config_from_env() ->
  #{
    host => os:getenv("DBHOST", "localhost"),
    port => list_to_integer(os:getenv("DBPORT", "5432")),
    database => require_env("DATABASE"),
    username => require_env("DBUSER"),
    password => require_env("DBPASSWORD")
  }.

start(_StartType, _StartArgs) ->
    DbConfig = db_config_from_env(),
    erl_bank_api_sup:start_link(DbConfig).

stop(_State) ->
    ok.
