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
-module(single).   
 
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
    
   % 1. Stop all host vms
    AllNodes=[list_to_atom(HostName++"@"++HostName)||HostName<-db_host_spec:get_all_hostnames()],
    StoppedHostNodes=[{Node,rpc:call(Node,init,stop,[])}||Node<-AllNodes],
    io:format("DBG: StoppedHostNodes ~p~n",[{StoppedHostNodes,?MODULE,?FUNCTION_NAME,?LINE}]),
    % 2. Create host vms and BaseDirs on the living servers
    AliveHosts=lists:sort(lib_host:which_servers_alive()),
    io:format("DBG: AliveHosts ~p~n",[{AliveHosts,?MODULE,?FUNCTION_NAME,?LINE}]),

   % [Host1|_]=AliveHosts,
%    StartInfo=start_check_hosts([Host1]),

    StartInfo=start_check_hosts(AliveHosts),
    Nodes=[N||{N,_}<-StartInfo],
    {[],[],[],[]}=check_all(Nodes),
 %   [rpc:call(Node,init,stop,[])||Node<-Nodes],

 %   []=[rpc:call(Node,db_application_spec,read_all,[])||Node<-Nodes],
 %   []=[rpc:call(Node,db_application_spec,read,[gitpath,"etcd.spec"])||Node<-Nodes],
    
    % Kill node and restart
    [{KilledNode,KilledHost}|_]=StartInfo,
    rpc:call(KilledNode,init,stop,[]),
    timer:sleep(2000),
    _StartInfo2=start_check_hosts([KilledHost]),
     {[],[],[],[]}=check_all([KilledNode]),
    
	

    %[rpc:call(Node,init,stop,[])||Node<-Nodes],
    io:format("TEST OK! ~p~n",[?MODULE]),
    timer:sleep(1000),
    ok.

check_all(Nodes)->
    DbAppSpec=[Node||Node<-Nodes,
	      {ok,"https://github.com/joq62/etcd.git"}=/=rpc:call(Node,db_application_spec,read,[gitpath,"etcd.spec"])],
    
    io:format("DBG: DbAppSpec ~p~n",[{DbAppSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
   % []=DbAppSpec,

    DbHostSpec=[Node||Node<-Nodes,
		      ["c100","c200","c201","c202","c300"]=/=lists:sort(rpc:call(Node,db_host_spec,get_all_hostnames,[]))],
    io:format("DBG: DbHostSpec ~p~n",[{DbHostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
   % []=DbHostSpec,

    DbDepl=[Node||Node<-Nodes,
	      {ok,["c202"]}=/=rpc:call(Node,db_deployments,read,[hosts,"solis"])],
    io:format("DBG: DbDepl ~p~n",[{DbDepl,?MODULE,?FUNCTION_NAME,?LINE}]),
   % []=DbDepl,
    DbDEplInfo=[Node||Node<-Nodes,
	      {ok,"solis.depl"}=/=rpc:call(Node,db_deployment_info,read,[name,"solis.depl"])],
   
   io:format("DBG: DbDEplInfo ~p~n",[{DbDEplInfo,?MODULE,?FUNCTION_NAME,?LINE}]),    
   % []=DbDEplInfo,
    {DbAppSpec,DbHostSpec,DbDepl,DbDEplInfo}.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start_check_hosts(AliveHosts)->
    io:format("DBG: AliveHosts ~p~n",[{AliveHosts,?MODULE,?FUNCTION_NAME,?LINE}]),
    start_check_host(AliveHosts,[]).
start_check_host([],R)->
    R;
start_check_host([HostName|T],Acc)->
    {ok,InitialNode,BaseDir}=lib_host:create_host_vm(HostName),
    {ok,ApplDir}=lib_host:git_load_host(InitialNode,BaseDir),
   % Delete Mnesia dirs
    RmMnesia=rpc:call(InitialNode,os,cmd,["rm -r Mnesia.*"]),  
    io:format("DBG: RmMnesia ~p~n",[{RmMnesia,?MODULE,?FUNCTION_NAME,?LINE}]),
    timer:sleep(2000),

    % Initial start of nodeCreate Schema 
    stopped=rpc:call(InitialNode,mnesia,stop,[]),
    DeleteSchema=rpc:call(InitialNode,mnesia,delete_schema,[[InitialNode]]),
    io:format("DBG:DeleteSchema ~p~n",[{DeleteSchema,?MODULE,?FUNCTION_NAME,?LINE}]),
    CreateSchema=rpc:call(InitialNode,mnesia,create_schema,[[InitialNode]]),
    io:format("DBG:CreateSchema ~p~n",[{CreateSchema,?MODULE,?FUNCTION_NAME,?LINE}]),

    ok=rpc:call(InitialNode,mnesia,start,[]),

    % Initiate tables
    ok=rpc:call(node(),db_application_spec,init_table,[node(),InitialNode],20*1000),
    {ok,"https://github.com/joq62/etcd.git"}=rpc:call(InitialNode,db_application_spec,read,[gitpath,"etcd.spec"]),
    io:format("DBG db_application_spec ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=rpc:call(node(),db_host_spec,init_table,[node(),InitialNode]),
    ["c100","c200","c201","c202","c300"]=lists:sort(rpc:call(InitialNode,db_host_spec,get_all_hostnames,[])),
    io:format("DBG db_host_spec ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=rpc:call(node(),db_deployments,init_table,[node(),InitialNode]),
    {ok,["c202"]}=rpc:call(InitialNode,db_deployments,read,[hosts,"solis"]),
    io:format("DBG db_deployments ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=rpc:call(node(),db_deployment_info,init_table,[node(),InitialNode]),
    {ok,"solis.depl"}=rpc:call(InitialNode,db_deployment_info,read,[name,"solis.depl"]),
    io:format("DBG db_deployment_info ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),


  io:format("DBG: ok,HostName,Node,BaseDir,ApplDir ~p~n",[{ok,HostName,InitialNode,BaseDir,ApplDir,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    
   start_check_host(T,[{InitialNode,HostName}|Acc]).

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
