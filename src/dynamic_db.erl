%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dynamic_db).    
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).

-export([
	 init/0
	 ]).


-define(StorageType,ram_disc).
-define(WAIT_FOR_TABLES,4*5000).
-define(TablesToCopy,[db_application_spec,db_deployment_info,db_deployments,db_host_spec]).
%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
init()->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    EtcdNodes=lists:delete(node(),sd:get_node(etcd)),
    start(EtcdNodes).

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start([])->
    % Add unique code to create the specific tables
 %% Create tables on TestNode
    R1={db_application_spec:create_table(),db_application_spec},
    R2={db_deployment_info:create_table(),db_deployment_info},
    R3={db_deployments:create_table(),db_deployments},
    R4={db_host_spec:create_table(),db_host_spec},
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
				 {"DBG: CreateTable   ",[R1,R2,R3,R4],?MODULE,node()}]), 
    [R1,R2,R3,R4];
start(EtcdNodes) ->
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
				 {"DBG: EtcdNodes   ",EtcdNodes,?MODULE,node()}]), 
    add_extra_nodes(EtcdNodes).


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
add_extra_nodes([Node|T])->
    case mnesia:change_config(extra_db_nodes,[Node]) of
	{ok,[Node]}->
	    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					 {"DBG: add_extra_nodes Node at node()  ",Node,?MODULE,node()}]), 
 
	    AddSchema=mnesia:add_table_copy(schema,node(),?StorageType),
	    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					 {"DBG: add_extra_nodes AddSchema  ",AddSchema,?MODULE,node()}]), 
%	    TablesFromNode=rpc:call(Node,mnesia,system_info,[tables]),
%	    [mnesia:add_table_copy(Table,node(),?StorageType)||Table<-TablesFromNode,
%							       Table/=schema],
	    AddTableCopies=[{Table,mnesia:add_table_copy(Table,node(),?StorageType)}||Table<-?TablesToCopy,
								 Table/=schema],
	    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					 {"DBG: add_extra_nodes AddTableCopies  ",AddTableCopies,?MODULE,node()}]), 
	    Tables=mnesia:system_info(tables),
	    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					 {"DBG: add_extra_nodes Tables  ",Tables,?MODULE,node()}]),
	    WaitForTables=mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES),
	    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					 {"DBG: add_extra_nodes WaitForTables  ",WaitForTables,?MODULE,node()}]);
	Reason->
	    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					 {"DBG: Didnt connect to Node Reason  ",Reason,?MODULE,node()}]),
	    add_extra_nodes(T)
    end.
    
