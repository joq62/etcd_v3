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
-module(dist_disc).   
 
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
  
    % 1. Init test setup
    io:format("1. Init test setup ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    InitStop=rpc:multicall([c100@c100,c200@c100,c201@c100,c202@c100,c300@c100],init,stop,[]),
    io:format("DBG InitStop ~p~n",[{InitStop,?MODULE,?FUNCTION_NAME,?LINE}]),
    RmMneisa_0=[os:cmd("rm -r "++"Mnesia."++atom_to_list(N))||N<-[c100@c100,c200@c100,c201@c100,c202@c100,c300@c100]],
    io:format("DBG RmMneisa_0 ~p~n",[{RmMneisa_0,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=setup(),
    Nodes=test_nodes:get_nodes(),
    [c100@c100,c200@c100,c201@c100,c202@c100,c300@c100]=Nodes, 
    [InitialNode|ExtraNodes]=Nodes,
    
    NodeNames=test_nodes:get_nodenames(),
    ["c100","c200","c201","c202","c300"]=NodeNames,
    
    %% 2. Intial install
    io:format("2. Intial install ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
   
    StorageType=disc_copies,
    MnesiaStop=rpc:multicall(Nodes,mnesia,stop,[]),
    io:format("DBG MnesiaStop ~p~n",[{MnesiaStop,?MODULE,?FUNCTION_NAME,?LINE}]),
    DeleteSchema=[rpc:call(Node,mnesia,delete_schema,[[Node]])||Node<-Nodes],
    io:format("DBG DeleteSchema ~p~n",[{DeleteSchema,?MODULE,?FUNCTION_NAME,?LINE}]),
%    MnesiaStart0=rpc:multicall(Nodes, mnesia,start, []),
    MnesiaStart0=[rpc:call(Node, mnesia,start, [])||Node<-[InitialNode]],
    io:format("DBG MnesiaStart0 ~p~n",[{MnesiaStart0,?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("2. running ~p~n",[{rpc:call(InitialNode,mnesia,system_info,[running_db_nodes]),?MODULE,?FUNCTION_NAME,?LINE}]),
%    io:format("2.test all  running ~p~n",[{[{Node,rpc:call(Node,mnesia,system_info,[running_db_nodes])}||Node<-Nodes],?MODULE,?FUNCTION_NAME,?LINE}])

    io:format("2. Intial install mnesia:System_info ~p~n",[{rpc:call(InitialNode,mnesia,system_info,[]),?MODULE,?FUNCTION_NAME,?LINE}]),
 %   rpc:multicall(Nodes, application, stop, [mnesia]),
    

    %% 3. Initiate tables 
    io:format(" 3. Initiate tables  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
 %% Start create tables on initial
    DbAppSpecCreate=rpc:call(InitialNode,db_application_spec,create_table,[]),
    io:format("DBG DbAppSpecCreate ~p~n",[{DbAppSpecCreate,?MODULE,?FUNCTION_NAME,?LINE}]),
    DbDepInfo=rpc:call(InitialNode,db_deployment_info,create_table,[]),
    io:format("DBG DbDepInfo ~p~n",[{DbDepInfo,?MODULE,?FUNCTION_NAME,?LINE}]),
    DbDeps=rpc:call(InitialNode,db_deployments,create_table,[]),
    io:format("DBG DbDeps ~p~n",[{DbDeps,?MODULE,?FUNCTION_NAME,?LINE}]),
    DbHostSpec=rpc:call(InitialNode,db_host_spec,create_table,[]),
    io:format("DBG DbHostSpec ~p~n",[{DbHostSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    % End create tables for etcd
    io:format("3.  mnesia:System_info ~p~n",[{rpc:call(InitialNode,mnesia,system_info,[]),?MODULE,?FUNCTION_NAME,?LINE}]),

    %% 4. Add info tables 
    io:format(" 4. Add info tables  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=add_info_tables(InitialNode),
    io:format( "4. Add info tables mnesia:System_info ~p~n",[{rpc:call(InitialNode,mnesia,system_info,[]),?MODULE,?FUNCTION_NAME,?LINE}]),
    %% 5. add nodes!!
    io:format("5. add nodes!! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    AddExtraNodes=add_extra_nodes(InitialNode,ExtraNodes,StorageType),
    io:format("DBG AddExtraNodes ~p~n",[{AddExtraNodes,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    io:format("5. add nodes!!  mnesia:System_info ~p~n",[{rpc:call(InitialNode,mnesia,system_info,[]),?MODULE,?FUNCTION_NAME,?LINE}]),
    
    % 6.Check all nodes first time
    io:format("6. Check all nodes first time ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
  
     ['c100@c100','c200@c100',
      'c201@c100','c202@c100','c300@c100']=lists:sort(rpc:call(InitialNode,mnesia,system_info,[running_db_nodes],5000)),
    ok=check_all(Nodes),



    %% 7. Kill Initial Node
    io:format("5 Kill initialnode test ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("DBG Kill Initial Node ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    RmMneisa=os:cmd("rm -r "++"Mnesia."++atom_to_list(InitialNode)),
    io:format("DBG RmMneisa  ~p~n",[{RmMneisa,?MODULE,?FUNCTION_NAME,?LINE}]),
    rpc:call(InitialNode,init,stop,[]),
    timer:sleep(10*1000),
   
    ['c200@c100',
     'c201@c100','c202@c100','c300@c100']=lists:sort(rpc:call(c200@c100,mnesia,system_info,[running_db_nodes])),

    [InitialNode]=[Node||Node<-Nodes,
			 {ok,"https://github.com/joq62/etcd.git"}=/=rpc:call(Node,db_application_spec,read,[gitpath,"etcd.spec"])],

    %% 8. Restart InitialNode
    io:format(" 8. Restart InitialNode ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,InitialNode}=test_nodes:start_slave("c100"),
    [_,SecondNode|_]=Nodes,
    AddIntialAgain=add_extra_nodes(SecondNode,[InitialNode],StorageType),
    io:format("DBG AddIntialAgain  ~p~n",[{AddIntialAgain,?MODULE,?FUNCTION_NAME,?LINE}]),
  %  rpc:call(InitialNode,mnesia,delete_schema,[[InitialNode]]),  
  %  rpc:call(InitialNode,mnesia,start,[]),  
%  ok=rpc:call(SecondNode,erlang,apply,[rpc,call,[InitialNode,mnesia,start,[]]]),
  %  Ping2= [rpc:call(Node,net_adm,ping,[InitialNode])||Node<-lists:delete(InitialNode,Nodes)],
  
%    io:format("DBG Ping2  ~p~n",[{Ping2,?MODULE,?FUNCTION_NAME,?LINE}]),
    WaitTable2=rpc:call(InitialNode,mnesia,wait_for_tables,[[db_application_spec], 10*1000]),
    io:format("DBG WaitTable2  ~p~n",[{WaitTable2,?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("DBG InitialNode mnesia,system_info,[]  ~p~n",[{rpc:call(InitialNode,mnesia,system_info,[]),?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("DBG SecondNode  mnesia,system_info,[]  ~p~n",[{rpc:call(SecondNode,mnesia,system_info,[]),?MODULE,?FUNCTION_NAME,?LINE}]),


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
add_info_tables(InitialNode)->
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
