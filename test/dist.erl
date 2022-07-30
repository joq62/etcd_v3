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
-module(dist).   
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    ok=setup(),

    install:init_node(),
    Nodes=install:hosts(),
    timer:sleep(3000),
    [rpc:call(Node,init,stop,[])||Node<-Nodes],

 %   []=[rpc:call(Node,db_application_spec,read_all,[])||Node<-Nodes],
 %   []=[rpc:call(Node,db_application_spec,read,[gitpath,"etcd.spec"])||Node<-Nodes],
    
   % []=[Node||Node<-Nodes,
%	      {ok,"https://github.com/joq62/etcd.git"}=/=rpc:call(Node,db_application_spec,read,[gitpath,"etcd.spec"])],
    
 %   []=[Node||Node<-Nodes,
%	      ["c100","c200","c201","c202","c300"]=/=lists:sort(rpc:call(Node,db_host_spec,get_all_hostnames,[]))],

 %   []=[Node||Node<-Nodes,
%	      {ok,["c202"]}=/=rpc:call(Node,db_deployments,read,[hosts,"solis"])],
%
 %   []=[Node||Node<-Nodes,
%	      {ok,"solis.depl"}=/=rpc:call(Node,db_deployment_info,read,[name,"solis.depl"])],
   
  
    %[rpc:call(Node,init,stop,[])||Node<-Nodes],
    io:format("TEST OK! ~p~n",[?MODULE]),
    timer:sleep(1000),
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
  
    % Simulate host
  %  R=rpc:call(node(),test_nodes,start_nodes,[],2000),
%    [Vm1|_]=test_nodes:get_nodes(),

%    Ebin="ebin",
 %   true=rpc:call(Vm1,code,add_path,[Ebin],5000),
 
   % R.
    ok.
