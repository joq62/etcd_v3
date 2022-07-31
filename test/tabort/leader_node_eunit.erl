%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(leader_node_eunit).   
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    ok=setup(),
    Nodes=test_nodes:get_nodes(),
    [c100@c100,c200@c100,c201@c100,c202@c100,c300@c100]=Nodes,  

    NodeNames=test_nodes:get_nodenames(),
    ["c100","c200","c201","c202","c300"]=NodeNames,
    ok=start_leader_node(Nodes,Nodes),
    rpc:call(c201@c100,leader_node,start_election,[]),
    
    timer:sleep(2000),
    c100@c100=rpc:call(c200@c100,leader_node,who_is_leader,[]),
   % shutdown_ok=rpc:call(c_0@c100,leader_server,stop,[],1000),
   % timer:sleep(2000),
%    N0=rpc:call(N5,leader_server,who_is_leader,[],200),
 %   rpc:cast(N0,init,stop,[]),
  %  timer:sleep(2000),  
  %  N1=rpc:call(N5,leader_server,who_is_leader,[],200),
  %  {ok,N0}=start_slave(NN0),
  %  ok=start_leader([N0],Nodes),
  %  timer:sleep(2000),  
  %  N0=rpc:call(N5,leader_server,who_is_leader,[],200),
    io:format("TEST OK! ~p~n",[?MODULE]),
 %   init:stop(),
    ok.

start_leader_node([],_)->
    ok;
start_leader_node([Node|T],Nodes)->
    ok=rpc:call(Node,application,load,[leader_node],5000),    
    ok=rpc:call(Node,application,set_env,[[{leader_node,[{nodes,Nodes}]}]],5000),
    ok=rpc:call(Node,application,start,[leader_node],5000),
    pong=rpc:call(Node,leader_node,ping,[]),
    start_leader_node(T,Nodes).
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

setup()->
    ok=test_nodes:start_nodes(),

    Nodes=test_nodes:get_nodes(),
    [c100@c100,c200@c100,c201@c100,c202@c100,c300@c100]=Nodes,  

    NodeNames=test_nodes:get_nodenames(),
    ["c100","c200","c201","c202","c300"]=NodeNames,
    ok.
