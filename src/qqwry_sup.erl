-module(qqwry_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([worker_names/0, worker/2]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    File = case application:get_env(qqwry, dbfile) of
               {ok, DbName} ->
                   erlang:list_to_atom(DbName);
               _ ->
                   'qqwry.dat'
           end,
    Processess = worker(worker_names(), File),
    {ok, { {one_for_one, 5, 10}, Processess}}.


worker_names() ->
    [qqwry_0,
     qqwry_1,
     qqwry_2,
     qqwry_3,
     qqwry_4,
     qqwry_5,
     qqwry_6,
     qqwry_7].


worker([], _) ->
    [];
worker([Name|T], File) ->
    [{Name, {qqwry, start_link, [Name, File]},
      permanent, 5000, worker, [qqwry]} | worker(T, File)].
