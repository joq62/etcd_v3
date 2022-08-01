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
    StorageType=ram_copies,
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

    HostStart=[lib_host:create_host_vm(HostName)||HostName<-AliveHosts],
    io:format("DBG: HostStart ~p~n",[{HostStart,?MODULE,?FUNCTION_NAME,?LINE}]),
    NodeHostList=[{Node,HostName}||{ok,Node,HostName}<-HostStart],
    io:format("DBG: NodeHostList ~p~n",[{NodeHostList,?MODULE,?FUNCTION_NAME,?LINE}]),
     
    % 2.1 load common and etcd on the running nodes {Node,HostName,BaseDir,ApplDir, {c202@c202,"c202","c202","/home/ubuntu/c202/host"},
    %% Common
   
    LoadStartCommon=[{load_start_appl(Node,HostName,"common.spec","common"),Node,HostName}||{Node,HostName}<-NodeHostList],
    io:format("DBG: LoadStartCommon ~p~n",[{LoadStartCommon,?MODULE,?FUNCTION_NAME,?LINE}]),

    LoadStartEtcd=[{load_start_appl(Node,HostName,"etcd.spec","etcd"),Node,HostName}||{Node,HostName}<-NodeHostList],
    io:format("DBG: LoadStartEtcd ~p~n",[{LoadStartEtcd,?MODULE,?FUNCTION_NAME,?LINE}]),



    % 3.On the IntialNode install load mnesia and table with content 
     io:format("3.On the IntialNode install load mnesia and table with content ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
   
    [{InitialNode,InitialHostName}|RestNodeHostsToBeAdded]=NodeHostList,
    ok=install_mnesia(InitialNode),
    ok=add_info_tables(TestNode,InitialNode),
    io:format("3. InitialNode mnesia:System_info ~p~n",[{InitialNode,rpc:call(InitialNode ,mnesia,system_info,[]),?MODULE,?FUNCTION_NAME,?LINE}]),
   

    % 4. Add nodes
    io:format("4. InitialNode starts mnesia on RestNodes  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    NodesToAdd=[Node||{Node,HostName}<-RestNodeHostsToBeAdded],
    io:format("DBG InitialNode starts mnesia on  NodesToAdd ~p~n",[{InitialNode,NodesToAdd,?MODULE,?FUNCTION_NAME,?LINE}]),
    AddExtraNodes=add_extra_nodes(InitialNode,NodesToAdd,StorageType),
    io:format("DBG AddExtraNodes ~p~n",[{AddExtraNodes,?MODULE,?FUNCTION_NAME,?LINE}]),
    
  

    % 6.Check all nodes first time
    io:format("6. Check all nodes first time ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    Nodes=[InitialNode|NodesToAdd],
    ok=check_all(Nodes),  

    %% 7. Kill Initial Node
    io:format("7. Kill initialnode test ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    rpc:call(InitialNode,init,stop,[]),
    timer:sleep(2000),
 

    [InitialNode]=[Node||Node<-Nodes,
			 {ok,"https://github.com/joq62/etcd.git"}=/=rpc:call(Node,db_application_spec,read,[gitpath,"etcd.spec"])],   

    SortedNodesToAdd=lists:sort(NodesToAdd),
    [NewInitialNode|_]=NodesToAdd,
    SortedNodesToAdd=lists:sort(rpc:call(NewInitialNode,mnesia,system_info,[running_db_nodes])),
    
    % 8. Restart InitialNode 
    io:format("8. Restart InitialNode  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,InitialNode,InitialHostName}=lib_host:create_host_vm(InitialHostName),
    LoadStartCommon1=load_start_appl(InitialNode,InitialHostName,"common.spec","common"),
    io:format("DBG: LoadStartCommon1 ~p~n",[{LoadStartCommon1,?MODULE,?FUNCTION_NAME,?LINE}]),
    LoadStartEtcd1=load_start_appl(InitialNode,InitialHostName,"etcd.spec","etcd"),
    io:format("DBG: LoadStartEtcd1 ~p~n",[{LoadStartEtcd1,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    AddExtraNodes1=add_extra_nodes(NewInitialNode,[InitialNode],StorageType),
    io:format("DBG AddExtraNodes1 ~p~n",[{AddExtraNodes1,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=check_all(Nodes),  
    
%    io:format("INIT STOP ************ ~p~n",[{rpc:call(TestNode ,init,stop,[]),?MODULE,?FUNCTION_NAME,?LINE}]),
%    timer:sleep(2000),
  
    io:format("TEST OK! ~p~n",[?MODULE]),

    ok.






%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
add_extra_nodes(InitialNode,NodesToAdd,StorageType)->
    [add_extra_node(InitialNode,NodeToAdd,StorageType)||NodeToAdd<-NodesToAdd].


%add_extra_node(_InitialNode,NodeToAdd,_StorageType,Result)->
 %   Result;   
add_extra_node(InitialNode,NodeToAdd,StorageType) ->
    io:format("DBG add_extra_node,InitialNode,NodeToAdd  ~p~n",[{InitialNode,NodeToAdd,?MODULE,?FUNCTION_NAME,?LINE}]), 
   stopped=rpc:call(NodeToAdd,mnesia,stop,[]),
    ok=rpc:call(NodeToAdd,mnesia,delete_schema,[[NodeToAdd]]),
    ok=rpc:call(NodeToAdd,mnesia,start,[]),
%    case rpc:call(InitialNode,mnesia,change_config,[extra_db_nodes,[NodeToAdd]]) of
    Result=case rpc:call(NodeToAdd,mnesia,change_config,[extra_db_nodes,[InitialNode]]) of
	       {ok,[InitialNode]}->
		   AddTableCopySchema=rpc:call(NodeToAdd,mnesia,add_table_copy,[schema,InitialNode,StorageType]),
		   io:format("DBG AddTableCopySchema ~p~n",[{AddTableCopySchema,?MODULE,?FUNCTION_NAME,?LINE}]),
		   
		   Tables=rpc:call(InitialNode,mnesia,system_info,[tables]),	  
		   AddTableCopies=[rpc:call(InitialNode,mnesia,add_table_copy,[Table,NodeToAdd,StorageType])||Table<-Tables,
													      Table/=schema],
		   io:format("DBG AddTableCopie ~p~n",[{AddTableCopies,?MODULE,?FUNCTION_NAME,?LINE}]),
		   WaitForTables=rpc:call(InitialNode,mnesia,wait_for_tables,[[Tables],20*1000]),
		   {ok, [connected,InitialNode,NodeToAdd,WaitForTables]};
	       Reason->
		   io:format("DBG Reason ~p~n",[{Reason,NodeToAdd,?MODULE,?FUNCTION_NAME,?LINE}]),
		   {error,[not_connected,InitialNode,NodeToAdd]}
	   
    end,
    Result.
		
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
    io:format("DBG:GitPath ~p~n",[{GitPath,?MODULE,?FUNCTION_NAME,?LINE}]),
    App=list_to_atom(ApplId),

    GitDir=filename:join(BaseDir,ApplId),
    RmGitDir=rpc:call(Node,os,cmd,["rm -rf "++GitDir]),
    io:format("DBG: RmGitDir ~p~n",[{GitDir,RmGitDir,?MODULE,?FUNCTION_NAME,?LINE}]),
    MakeGitDir=rpc:call(Node,file,make_dir,[GitDir]),
    io:format("DBG: MakeGitDir ~p~n",[{MakeGitDir,?MODULE,?FUNCTION_NAME,?LINE}]),
    GitClone=rpc:call(node(),appl,git_clone_to_dir,[Node,GitPath,GitDir]),
    io:format("DBG: GitClone ~p~n",[{GitClone,?MODULE,?FUNCTION_NAME,?LINE}]),

    Load=rpc:call(node(),appl,load,[Node,App,[filename:join(GitDir,"ebin")]]),
    io:format("DBG: Load ~p~n",[{Load,?MODULE,?FUNCTION_NAME,?LINE}]),

    Start=rpc:call(node(),appl,start,[Node,App]),
    io:format("DBG: Start ~p~n",[{Start,?MODULE,?FUNCTION_NAME,?LINE}]),
    pong=rpc:call(Node,App,ping,[]),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start_check_hosts(AliveHosts)->
    io:format("DBG: InitialNode,Rest ~p~n",[{AliveHosts,?MODULE,?FUNCTION_NAME,?LINE}]),
    %% create host vm and load etcd obs , commom etc is not loaded 
    NodeHostBaseApplDirList=load_host(AliveHosts,[]),
    io:format("DBG: NodeHostBaseApplDirList  ~p~n",[{NodeHostBaseApplDirList,?MODULE,?FUNCTION_NAME,?LINE}]),
    NodeHostBaseApplDirList.

load_host([],NodeHostBaseApplDirList)->
    NodeHostBaseApplDirList;
load_host([HostName|T],Acc)->
    {ok,Node,BaseDir}=lib_host:create_host_vm(HostName),
    {ok,ApplDir}=lib_host:git_load_host(Node,BaseDir),
   % Delete Mnesia dirs
    RmMnesia=rpc:call(Node,os,cmd,["rm -r Mnesia.*"]),  
    io:format("DBG: RmMnesia ~p~n",[{RmMnesia,?MODULE,?FUNCTION_NAME,?LINE}]),
    timer:sleep(2000),
    load_host(T,[{Node,HostName,BaseDir,ApplDir}|Acc]).

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
install_mnesia(Node)->
    % 0. Install test node
    io:format("0. Install test node  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    %% Start Mnesia
    MnesiaStopTest=rpc:call(Node,mnesia,stop,[]),
    io:format("DBG MnesiaStopTest ~p~n",[{MnesiaStopTest,?MODULE,?FUNCTION_NAME,?LINE}]),
    DeleteSchemaTest=rpc:call(Node,mnesia,delete_schema,[[Node]]),
    io:format("DBG DeleteSchemaTest ~p~n",[{DeleteSchemaTest,?MODULE,?FUNCTION_NAME,?LINE}]),
    MnesiaStartTest=rpc:call(Node, mnesia,start, []),
    io:format("DBG MnesiaStartTest ~p~n",[{MnesiaStartTest,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    %% Create tables on TestNode
    DbAppSpecCreate=rpc:call(Node,db_application_spec,create_table,[]),
    io:format("DBG DbAppSpecCreate ~p~n",[{DbAppSpecCreate,?MODULE,?FUNCTION_NAME,?LINE}]),
    DbDepInfo=rpc:call(Node,db_deployment_info,create_table,[]),
    io:format("DBG DbDepInfo ~p~n",[{DbDepInfo,?MODULE,?FUNCTION_NAME,?LINE}]),
    DbDeps=rpc:call(Node,db_deployments,create_table,[]),
    io:format("DBG DbDeps ~p~n",[{DbDeps,?MODULE,?FUNCTION_NAME,?LINE}]),
    DbHostSpec=rpc:call(Node,db_host_spec,create_table,[]),
    io:format("DBG DbHostSpec ~p~n",[{DbHostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    % End create tables for etcd
    io:format("0. Install test node  mnesia:System_info ~p~n",[{rpc:call(Node ,mnesia,system_info,[]),?MODULE,?FUNCTION_NAME,?LINE}]),
    ok.

install_test_node(TestNode)->
  % 0. Install test node
    io:format("0. Install test node  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    %% Start Mnesia
    MnesiaStopTest=rpc:call(TestNode,mnesia,stop,[]),
    io:format("DBG MnesiaStopTest ~p~n",[{MnesiaStopTest,?MODULE,?FUNCTION_NAME,?LINE}]),
    DeleteSchemaTest=rpc:call(TestNode,mnesia,delete_schema,[[TestNode]]),
    io:format("DBG DeleteSchemaTest ~p~n",[{DeleteSchemaTest,?MODULE,?FUNCTION_NAME,?LINE}]),
    MnesiaStartTest=rpc:call(TestNode, mnesia,start, []),
    io:format("DBG MnesiaStartTest ~p~n",[{MnesiaStartTest,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    %% Create tables on TestNode
    DbAppSpecCreate=rpc:call(TestNode,db_application_spec,create_table,[]),
    io:format("DBG DbAppSpecCreate ~p~n",[{DbAppSpecCreate,?MODULE,?FUNCTION_NAME,?LINE}]),
    DbDepInfo=rpc:call(TestNode,db_deployment_info,create_table,[]),
    io:format("DBG DbDepInfo ~p~n",[{DbDepInfo,?MODULE,?FUNCTION_NAME,?LINE}]),
    DbDeps=rpc:call(TestNode,db_deployments,create_table,[]),
    io:format("DBG DbDeps ~p~n",[{DbDeps,?MODULE,?FUNCTION_NAME,?LINE}]),
    DbHostSpec=rpc:call(TestNode,db_host_spec,create_table,[]),
    io:format("DBG DbHostSpec ~p~n",[{DbHostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    % End create tables for etcd

    %% add_info_tables(TestNode)
    ok=add_info_tables(TestNode),

    io:format("0. Install test node  mnesia:System_info ~p~n",[{rpc:call(TestNode ,mnesia,system_info,[]),?MODULE,?FUNCTION_NAME,?LINE}]),
    ok.
    
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
