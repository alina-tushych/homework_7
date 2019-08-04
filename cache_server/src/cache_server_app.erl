-module(cache_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_, _) ->
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(ssl),
    ok = application:ensure_started(ranch),
    ok = application:ensure_started(cowlib),
    ok = application:ensure_started(cowboy),
    ok = application:ensure_started(jsx),
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/api/cache_server", cache_server_http, []}
		]}
	]),

	{ok, _Pid} = cowboy:start_clear(http, [{port, 8080}], #{env => #{dispatch => Dispatch}}), 
    cache_server_sup:start_link().

stop(_State) ->
    ok.
