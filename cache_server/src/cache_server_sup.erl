-module(cache_server_sup).

-behaviour(supervisor).

-include("../include/cache_server.hrl").

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    T = {cache_server, {cache_server, start_link, [?TABLENAME, [{drop_interval, ?DROPINTERVAL}]]}, permanent, 2000, worker, [cache_server]},
    {ok, {{one_for_one, 500, 25}, [T]}}.

