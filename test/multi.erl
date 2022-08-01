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
-module(multi).   
 
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
    TestNode=node(),

    % 0. Install the test node , mnesia is configured with data 
    ok=install_test_node(TestNode),

    % 1. Stop all host vms. SetUp Ensure that all nodes are killed create vms
    AllNodes=[list_to_atom(HostName++"@"++HostName)||HostName<-db_host_spec:get_all_hostnames()],
    StoppedHostNodes=[{Node,rpc:call(Node,init,stop,[])}||Node<-AllNodes],
    io:format("DBG: StoppedHostNodes ~p~n",[{StoppedHostNodes,?MODULE,?FUNCTION_NAME, ?LINE}]),
   
    % 2. Create host vms and BaseDirs on the living servers
    AliveHosts=lists:sort(lib_host:which_servers_alive()),
    io:format("DBG: AliveHosts ~p~n",[{AliveHosts,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    %% Create host vm
    % create_vm(HostName,BaseDir,NodeName,TimeOut,Cookie)
    TimeOut=7000,    
    Cookie=atom_to_list(erlang:get_cookie()),
    HostStart=[create_vm_via_host(HostName,HostName,HostName,TimeOut,Cookie)||HostName<-AliveHosts],
    io:format("DBG: HostStart ~p~n",[{HostStart,?MODULE,?FUNCTION_NAME,?LINE}]),
    NodeHostList=[{Node,HostName}||{ok,Node,HostName}<-HostStart],
    io:format("DBG: NodeHostList ~p~n",[{NodeHostList,?MODULE,?FUNCTION_NAME,?LINE}]),

    Nodes=[Node||{Node,_}<-NodeHostList],

    % Ensure that nodes are connected
    [{InitialNode,IntialHostName}|RestToStart]=NodeHostList,
    Ping=[{Node,rpc:call(InitialNode,net_adm,ping,[Node])}||{Node,_}<-RestToStart],
    io:format("DBG: Ping ~p~n",[{Ping,?MODULE,?FUNCTION_NAME,?LINE}]),
    % 2.1 load common and etcd on the running nodes {Node,HostName,BaseDir,ApplDir, 
    % {c202@c202,"c202","c202","/home/ubuntu/c202/host"},
   
    % 3.1 Start Initial Node
    LoadStartEtcdIntialNode=load_start_appl(InitialNode,IntialHostName,"etcd.spec","etcd"),
    io:format("DBG: LoadStartEtcdIntialNode ~p~n",[{LoadStartEtcdIntialNode,?MODULE,?FUNCTION_NAME,?LINE}]),    
    io:format("3.1 InitialNode mnesia:System_info ~p~n",[{InitialNode,rpc:call(InitialNode,mnesia,system_info,[]),?MODULE,?FUNCTION_NAME,?LINE}]),

    io:format("DBG: nodes() ~p~n",[{rpc:call(InitialNode,erlang,nodes,[]),?MODULE,?FUNCTION_NAME,?LINE}]), 

    LoadStartEtcd=[{load_start_appl(Node,HostName,"etcd.spec","etcd"),Node,HostName}||{Node,HostName}<-RestToStart],
    io:format("DBG: LoadStartEtcd ~p~n",[{LoadStartEtcd,?MODULE,?FUNCTION_NAME,?LINE}]), 
  %  timer:sleep(2*5000),

    io:format("3.2 Check if RestToStart are started mnesia:System_info ~p~n",[{[{Node,rpc:call(Node,mnesia,system_info,[])}||{Node,_}<-NodeHostList],?MODULE,?FUNCTION_NAME,?LINE}]),   
  
    ok=add_info_tables(TestNode,InitialNode),
    io:format("3. InitialNode mnesia:System_info ~p~n",[{InitialNode,rpc:call(InitialNode ,mnesia,system_info,[]),?MODULE,?FUNCTION_NAME,?LINE}]),
   
    ok=check_all(Nodes),
    
    %% 7. Kill Initial Node
    io:format("7. Kill initialnode test ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    rpc:call(InitialNode,init,stop,[]),
    timer:sleep(2000),
 

    [InitialNode]=[Node||Node<-Nodes,
			 {ok,"https://github.com/joq62/etcd.git"}=/=rpc:call(Node,db_application_spec,read,[gitpath,"etcd.spec"])],   
    RestNodes=lists:sort([Node||{Node,_HostName}<-RestToStart]),
    [SecondNode|_]=RestNodes,
    RestNodes=lists:sort(rpc:call(SecondNode ,mnesia,system_info,[running_db_nodes])),
    io:format("7. runnings nodes when IntialNode is killed ~p~n",[{rpc:call(SecondNode ,mnesia,system_info,[running_db_nodes]),?MODULE,?FUNCTION_NAME,?LINE}]),

    % 8. Restart InitialNode 

 % create_vm(HostName,BaseDir,NodeName,TimeOut,Cookie)
    TimeOut=7000,    
    Cookie=atom_to_list(erlang:get_cookie()),
    HostStartInitialNode=create_vm_via_host(IntialHostName,IntialHostName,IntialHostName,TimeOut,Cookie),
    io:format("DBG: HostStartInitialNode ~p~n",[{HostStartInitialNode,?MODULE,?FUNCTION_NAME,?LINE}]), 

 %   Ping=[{Node,rpc:call(InitialNode,net_adm,ping,[Node])}||Node<-Nodes],
    LoadStartEtcdIntialNode=load_start_appl(InitialNode,IntialHostName,"etcd.spec","etcd"),
  
    ok=check_all(Nodes),  
    
%    io:format("INIT STOP ************ ~p~n",[{rpc:call(TestNode ,init,stop,[]),?MODULE,?FUNCTION_NAME,?LINE}]),
%    timer:sleep(2000),

    io:format("Finale o ~p~n",[{InitialNode,rpc:call(InitialNode ,mnesia,system_info,[]),?MODULE,?FUNCTION_NAME,?LINE}]),
  
    io:format("TEST OK! ~p~n",[?MODULE]),

    ok.





%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
create_vm_via_host(HostName,BaseDir,NodeName,TimeOut,Cookie)->
    {HostName,Ip,_,Port,User,Password,_}=db_host_spec:read(HostName),
    my_ssh:ssh_send(Ip,Port,User,Password,"rm -rf "++BaseDir,TimeOut),
    my_ssh:ssh_send(Ip,Port,User,Password,"mkdir "++BaseDir,TimeOut),
    PaArgs="  ",
    EnvArgs=" ",
    {ok,Node}=vm:ssh_create(HostName,NodeName,Cookie,PaArgs,EnvArgs,
			    {Ip,Port,User,Password,TimeOut}),  
    {ok,Node,HostName}.
		
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
add_info_tables(SourceNode,DestNode)->
     % Init dbases
    true=rpc:call(SourceNode,code,add_patha,["config/ebin"]),
    rpc:call(SourceNode,application,start,[config]),
    %% init 
    ok=rpc:call(SourceNode,db_application_spec,init_table,[SourceNode,DestNode]),
    {ok,"https://github.com/joq62/etcd.git"}=rpc:call(DestNode,db_application_spec,read,[gitpath,"etcd.spec"]),
    
    ok=rpc:call(SourceNode,db_host_spec,init_table,[SourceNode,DestNode]),
    ["c100","c200","c201","c202","c300"]=lists:sort(rpc:call(DestNode,db_host_spec,get_all_hostnames,[])),

    ok=rpc:call(SourceNode,db_deployments,init_table,[SourceNode,DestNode]),
    {ok,["c202"]}=rpc:call(DestNode,db_deployments,read,[hosts,"solis"]),

    ok=rpc:call(SourceNode,db_deployment_info,init_table,[SourceNode,DestNode]),
    {ok,"solis.depl"}=rpc:call(DestNode,db_deployment_info,read,[name,"solis.depl"]),
    
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
add_info_tables(InitialNode)->
     % Init dbases
    true=rpc:call(InitialNode,code,add_patha,["config/ebin"]),
    ok=rpc:call(InitialNode,application,start,[config]),
    %% init 
    ok=rpc:call(InitialNode,db_application_spec,init_table,[InitialNode,InitialNode]),
    {ok,"https://github.com/joq62/etcd.git"}=rpc:call(InitialNode,db_application_spec,read,[gitpath,"etcd.spec"]),
    
    ok=rpc:call(InitialNode,db_host_spec,init_table,[InitialNode,InitialNode]),
    ["c100","c200","c201","c202","c300"]=lists:sort(rpc:call(InitialNode,db_host_spec,get_all_hostnames,[])),

    ok=rpc:call(InitialNode,db_deployments,init_table,[InitialNode,InitialNode]),
    {ok,["c202"]}=rpc:call(InitialNode,db_deployments,read,[hosts,"solis"]),

    ok=rpc:call(InitialNode,db_deployment_info,init_table,[InitialNode,InitialNode]),
    {ok,"solis.depl"}=rpc:call(InitialNode,db_deployment_info,read,[name,"solis.depl"]),
    
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
check_all(Nodes)->
    
      []=[Node||Node<-Nodes,
	      {ok,"https://github.com/joq62/etcd.git"}=/=rpc:call(Node,db_application_spec,read,[gitpath,"etcd.spec"])],
    
    []=[Node||Node<-Nodes,
	      ["c100","c200","c201","c202","c300"]=/=lists:sort(rpc:call(Node,db_host_spec,get_all_hostnames,[]))],

    []=[Node||Node<-Nodes,
	      {ok,["c202"]}=/=rpc:call(Node,db_deployments,read,[hosts,"solis"])],

    []=[Node||Node<-Nodes,
	      {ok,"solis.depl"}=/=rpc:call(Node,db_deployment_info,read,[name,"solis.depl"])],
   
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
load_start_appl(Node,BaseDir,ApplSpec,ApplId)->
    {ok,GitPath}=rpc:call(node(),db_application_spec,read,[gitpath,ApplSpec]),
 %   io:format("DBG:GitPath ~p~n",[{GitPath,?MODULE,?FUNCTION_NAME,?LINE}]),
    App=list_to_atom(ApplId),

    GitDir=filename:join(BaseDir,ApplId),
    []=rpc:call(Node,os,cmd,["rm -rf "++GitDir]),
  %  io:format("DBG: RmGitDir ~p~n",[{GitDir,RmGitDir,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=rpc:call(Node,file,make_dir,[GitDir]),
   % io:format("DBG: MakeGitDir ~p~n",[{MakeGitDir,?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,_}=rpc:call(node(),appl,git_clone_to_dir,[Node,GitPath,GitDir]),
   % io:format("DBG: GitClone ~p~n",[{GitClone,?MODULE,?FUNCTION_NAME,?LINE}]),

    ok=rpc:call(node(),appl,load,[Node,App,[filename:join(GitDir,"ebin")]]),
   % io:format("DBG: Load ~p~n",[{Load,?MODULE,?FUNCTION_NAME,?LINE}]),

    ok=rpc:call(node(),appl,start,[Node,App]),
    %io:format("DBG: Start ~p~n",[{Start,?MODULE,?FUNCTION_NAME,?LINE}]),
    pong=rpc:call(Node,App,ping,[]),
    ok.



install_test_node(TestNode)->
  % 0. Install test node
    io:format("0. Install test node  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    %% Start Mnesia
    stopped=rpc:call(TestNode,mnesia,stop,[]),
 %   io:format("DBG MnesiaStopTest ~p~n",[{MnesiaStopTest,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=rpc:call(TestNode,mnesia,delete_schema,[[TestNode]]),
  %  io:format("DBG DeleteSchemaTest ~p~n",[{DeleteSchemaTest,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=rpc:call(TestNode, mnesia,start, []),
%    io:format("DBG MnesiaStartTest ~p~n",[{MnesiaStartTest,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    %% Create tables on TestNode
    ok=rpc:call(TestNode,db_application_spec,create_table,[]),
 %   io:format("DBG DbAppSpecCreate ~p~n",[{DbAppSpecCreate,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=rpc:call(TestNode,db_deployment_info,create_table,[]),
   % io:format("DBG DbDepInfo ~p~n",[{DbDepInfo,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=rpc:call(TestNode,db_deployments,create_table,[]),
  %  io:format("DBG DbDeps ~p~n",[{DbDeps,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=rpc:call(TestNode,db_host_spec,create_table,[]),
  %  io:format("DBG DbHostSpec ~p~n",[{DbHostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    % End create tables for etcd

    %% add_info_tables(TestNode)
    ok=add_info_tables(TestNode),

%    io:format("0. Install test node  mnesia:System_info ~p~n",[{rpc:call(TestNode ,mnesia,system_info,[]),?MODULE,?FUNCTION_NAME,?LINE}]),
    ok.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

setup()->
  
    ok.
