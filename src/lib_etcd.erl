%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_etcd).    
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).

-export([
	 dynamic_install_start/1,
	 dynamic_install/2,
	 load_textfile/1,
	 restart/0,
	 dynamic_db_init/1,
	 dynamic_add_table/2
	 ]).
%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
dynamic_install_start(IntialNode)->
    stopped=rpc:call(IntialNode,mnesia,stop,[]),
    {aborted,{node_not_running,IntialNode}}=rpc:call(IntialNode,mnesia,del_table_copy,[schema,IntialNode]),
    ok=rpc:call(IntialNode,mnesia,delete_schema,[[IntialNode]]),
    ok=rpc:call(IntialNode,mnesia,create_schema,[[IntialNode]]),
    ok=rpc:call(IntialNode,mnesia,start,[]).
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
dynamic_install([],_IntialNode)->
    ok;
dynamic_install([NodeToAdd|T],IntialNode)->
    stopped=rpc:call(NodeToAdd,mnesia,stop,[]),
    {aborted,{node_not_running,NodeToAdd}}=rpc:call(NodeToAdd,mnesia,del_table_copy,[schema,IntialNode]),
    ok=rpc:call(NodeToAdd,mnesia,delete_schema,[[NodeToAdd]]),
    ok=rpc:call(NodeToAdd,mnesia,start,[]),
    case rpc:call(IntialNode,mnesia,change_config,[extra_db_nodes,[NodeToAdd]],5000) of
	{ok,[NodeToAdd]}->
	    {atomic,_}=rpc:call(IntialNode,mnesia,change_table_copy_type,[schema,NodeToAdd,disc_copies]),
	    Tables=rpc:call(IntialNode,mnesia,system_info,[tables]),	  
	    [{atomic,_}=rpc:call(IntialNode,mnesia,add_table_copy,[Table,NodeToAdd,disc_copies])||Table<-Tables,
											Table/=schema],
	    rpc:call(IntialNode,mnesia,wait_for_tables,[Tables],20*1000),
	    ok;
	Reason ->
	    io:format("NodeToAdd,IntialNode,Reason ~p~n",[{NodeToAdd,IntialNode,Reason,?FUNCTION_NAME,?MODULE,?LINE}]),
	    dynamic_install(T,IntialNode) 
    end.
  %  dynamic_install(T,IntialNode).


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
load_textfile(TableTextFiles)->
    %% Missing tables 
    PresentTables=[Table||Table<-mnesia:system_info(tables),
			  true=:=lists:keymember(Table,1,TableTextFiles),
			  Table/=schema],
 %   io:format("PresentTables  ~p~n",[{PresentTables,node(),?FUNCTION_NAME,?MODULE,?LINE}]),
    
    LoadInfoRes=[{mnesia:load_textfile(TextFile),Table,TextFile}||{Table,_StorageType,TextFile}<-TableTextFiles,
						      false=:=lists:member(Table,PresentTables)],
 %   io:format("LoadInfo ~p~n",[{LoadInfo,node(),?FUNCTION_NAME,?MODULE,?LINE}]),
    AddTableRes=[{N,Table,rpc:call(N,dbase,dynamic_add_table,[Table,StorageType],5000)}||N<-lists:delete(node(),sd:get(dbase_infra)),
											{Table,StorageType,_TextFile}<-TableTextFiles],
    {AddTableRes,LoadInfoRes}.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
restart()->
    mnesia:stop(),   
    mnesia:start().   




dynamic_db_init([])->

 %   io:format(" ~p~n",[{node(),?FUNCTION_NAME,?MODULE,?LINE}]),
    mnesia:stop(),
  %  mnesia:del_table_copy(schema,node()),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([]),
    mnesia:start(),   
    ok;

dynamic_db_init([DbaseNode|T])->
    io:format(" ~p~n",[{DbaseNode,node(),?FUNCTION_NAME,?MODULE,?LINE}]),
  %  stopped=rpc:call(DbaseNode,mnesia,stop,[]),
  %  ok=rpc:call(DbaseNode,mnesia,del_table_copy,[schema,DbaseNode]),
  %  ok=rpc:call(DbaseNode,mnesia,delete_schema,[[DbaseNode]]),
 %   ok=rpc:call(DbaseNode,mnesia,create,[]),
 %   ok=rpc:call(DbaseNode,mnesia,start,[]),
  %  mnesia:stop(),
  %  mnesia:del_table_copy(schema,node()),
  %  mnesia:delete_schema([node()]),
    mnesia:create_schema([]),
    mnesia:start(),   
%io:format("DbaseNode dynamic_db_init([DbaseNode|T]) ~p~n",[{DbaseNode,node(),?FUNCTION_NAME,?MODULE,?LINE}]),
    StorageType=ram_copies,
    case rpc:call(node(),mnesia,change_config,[extra_db_nodes,[DbaseNode]],5000) of
	{ok,[DbaseNode]}->
	    mnesia:add_table_copy(schema,node(),ram_copies),
	    Tables=mnesia:system_info(tables),
	    R_add_table_copy=[mnesia:add_table_copy(Table, node(),StorageType)||Table<-Tables,
							       Table/=schema],
	    io:format(" R_add_table_copy ~p~n",[{ R_add_table_copy,?FUNCTION_NAME,?MODULE,?LINE}]),
	    mnesia:wait_for_tables(Tables,20*1000),
	    ok;
	Reason ->
	    io:format("Reason ~p~n",[{Reason,?FUNCTION_NAME,?MODULE,?LINE}]),
	    dynamic_db_init(T)
    end.
  



dynamic_add_table(Table,StorageType)->
  %  io:format("Module ~p~n",[{Module,node(),?FUNCTION_NAME,?MODULE,?LINE}]),
    AddedNode=node(),
    T_result=mnesia:add_table_copy(Table, AddedNode, StorageType),
 %   io:format("T_result ~p~n",[{T_result,node(),?FUNCTION_NAME,?MODULE,?LINE}]),
    Tables=mnesia:system_info(tables),
    mnesia:wait_for_tables(Tables,20*1000),
    T_result.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
