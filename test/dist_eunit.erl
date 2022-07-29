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
-module(dist_eunit).   
 
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
    [InitialNode|_]=Nodes,
    
    NodeNames=test_nodes:get_nodenames(),
    ["c100","c200","c201","c202","c300"]=NodeNames,
    
    %% Intial install
    rpc:call(InitialNode,etcd_app,install,[Nodes]),
    [rpc:call(Node,application,start,[etcd])||Node<-Nodes],
    rpc:call(InitialNode,mnesia,wait_for_tables,[[db_application_spec], 10*1000]),
  %  io:format("InitialNode: mnesia:system_info() ~p~n",[lists:sort(rpc:call(InitialNode,mnesia,system_info,[tables]))]),
    
    ['c100@c100','c200@c100',
     'c201@c100','c202@c100','c300@c100']=lists:sort(rpc:call(InitialNode,mnesia,system_info,[running_db_nodes])),
    % Init dbases
    true=rpc:call(InitialNode,code,add_patha,["config/ebin"]),
    ok=rpc:call(InitialNode,application,start,[config]),
    %% init 
    ok=rpc:call(InitialNode,db_application_spec,init_table,[InitialNode,InitialNode]),
    {ok,"https://github.com/joq62/etcd.git"}=rpc:call(c100@c100,db_application_spec,read,[gitpath,"etcd.spec"]),
    
    ok=rpc:call(InitialNode,db_host_spec,init_table,[InitialNode,InitialNode]),
    ["c100","c200","c201","c202","c300"]=lists:sort(rpc:call(InitialNode,db_host_spec,get_all_hostnames,[])),

    ok=rpc:call(InitialNode,db_deployments,init_table,[InitialNode,InitialNode]),
    {ok,["c202"]}=rpc:call(InitialNode,db_deployments,read,[hosts,"solis"]),

    ok=rpc:call(InitialNode,db_deployment_info,init_table,[InitialNode,InitialNode]),
    {ok,"solis.depl"}=rpc:call(InitialNode,db_deployment_info,read,[name,"solis.depl"]),
    
    []=[Node||Node<-Nodes,
	      {ok,"https://github.com/joq62/etcd.git"}=/=rpc:call(Node,db_application_spec,read,[gitpath,"etcd.spec"])],
    
    []=[Node||Node<-Nodes,
	      ["c100","c200","c201","c202","c300"]=/=lists:sort(rpc:call(InitialNode,db_host_spec,get_all_hostnames,[]))],

    []=[Node||Node<-Nodes,
	      {ok,["c202"]}=/=rpc:call(InitialNode,db_deployments,read,[hosts,"solis"])],

    []=[Node||Node<-Nodes,
	      {ok,"solis.depl"}=/=rpc:call(InitialNode,db_deployment_info,read,[name,"solis.depl"])],
   

    %% Kill Initial Node

    ok=rpc:call(InitialNode,init,stop,[]),
    timer:sleep(2000),
   
    ['c200@c100',
     'c201@c100','c202@c100','c300@c100']=lists:sort(rpc:call(c200@c100,mnesia,system_info,[running_db_nodes])),

    [InitialNode]=[Node||Node<-Nodes,
			 {ok,"https://github.com/joq62/etcd.git"}=/=rpc:call(Node,db_application_spec,read,[gitpath,"etcd.spec"])],

    %% Restart InitialNode

    {ok,InitialNode}=test_nodes:start_slave("c100"),
    ok=rpc:call(InitialNode,application,start,[etcd]),
    rpc:call(InitialNode,mnesia,wait_for_tables,[[db_application_spec], 10*1000]),

     []=[Node||Node<-Nodes,
	       {ok,"https://github.com/joq62/etcd.git"}=/=rpc:call(Node,db_application_spec,read,[gitpath,"etcd.spec"])],

%% 


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
  %  init:stop(),
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
