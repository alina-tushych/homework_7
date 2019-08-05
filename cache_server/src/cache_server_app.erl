-module(cache_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_, _) ->
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/api/cache_server", cache_server_http, []}
		]}
	]),

	{ok, _Pid} = cowboy:start_clear(http, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    cache_server_sup:start_link().

stop(_State) ->
    ok.
