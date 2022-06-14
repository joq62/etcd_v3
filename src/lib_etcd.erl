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
	 load_textfile/1,
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
dynamic_db_init([])->
 %   io:format(" ~p~n",[{node(),?FUNCTION_NAME,?MODULE,?LINE}]),
    mnesia:stop(),
    mnesia:del_table_copy(schema,node()),
    mnesia:delete_schema([node()]),
    mnesia:start(),   
    ok;

dynamic_db_init([DbaseNode|T])->
%    io:format("DbaseNode dynamic_db_init([DbaseNode|T]) ~p~n",[{DbaseNode,node(),?FUNCTION_NAME,?MODULE,?LINE}]),
    mnesia:stop(),
    mnesia:del_table_copy(schema,node()),
    mnesia:delete_schema([node()]),
    mnesia:start(),
%io:format("DbaseNode dynamic_db_init([DbaseNode|T]) ~p~n",[{DbaseNode,node(),?FUNCTION_NAME,?MODULE,?LINE}]),
    StorageType=ram_copies,
  %  case rpc:call(DbaseNode,mnesia,change_config,[extra_db_nodes, [node()]],5000) of
    case rpc:call(node(),mnesia,change_config,[extra_db_nodes,[DbaseNode]],5000) of
	{ok,[_AddedNode]}->
	    Tables=mnesia:system_info(tables),
	    [mnesia:add_table_copy(Table, node(),StorageType)||Table<-Tables,
							       Table/=schema],
	    mnesia:wait_for_tables(Tables,20*1000),
	    ok;
	_Reason ->
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
