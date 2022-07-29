%%%-------------------------------------------------------------------
%% @doc etcd public API
%% @end
%%%-------------------------------------------------------------------

-module(etcd_app).

-behaviour(application).

-include("db_application_spec.hrl").

-export([start/2, stop/1]).
-export([install/1]).

start(normal, _StartArgs) ->
    etcd_sup:start_link().

stop(_State) ->
    ok.

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    %% Start create tables for etcd
    db_application_spec:create_table(Nodes),
    db_deployment_info:create_table(Nodes),
    db_deployments:create_table(Nodes),
    db_host_spec:create_table(Nodes),
    % End create tables for etcd
    rpc:multicall(Nodes, application, stop, [mnesia]).
%% internal functions
