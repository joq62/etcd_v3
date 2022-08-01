-module(db_application_spec).
-import(lists, [foreach/2]).
-compile(export_all).

-include("db_application_spec.hrl").
-include_lib("stdlib/include/qlc.hrl").

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}
				]),
    mnesia:wait_for_tables([?TABLE], 20000).

create_table(NodeList,StorageType)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {StorageType,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

add_node(Node,StorageType)->
    Result=case mnesia:change_config(extra_db_nodes, [Node]) of
	       {ok,[Node]}->
		   AddSchema=mnesia:add_table_copy(schema, node(),StorageType),
		   rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
						{"DBG:AddSchema  ",AddSchema,node()}]),
		   AddTableCopies=mnesia:add_table_copy(?TABLE, node(), StorageType),
		   rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
						{"DBG: AddTableCopies  ",AddTableCopies,node()}]),
		   Tables=mnesia:system_info(tables),
		   rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
						{"DBG: Tables  ",Tables,node()}]),
		   WaitForTables=mnesia:wait_for_tables(Tables,20*1000),
		   rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
						{"DBG: WaitForTables  ",WaitForTables,node()}]);
	       Reason ->
		   Reason
	   end,
    Result.

create(Name,Vsn,GitPath,Cmd)->
    Record=#?RECORD{
		    name=Name,
		    vsn=Vsn,
		    gitpath=GitPath,
		    cmd=Cmd
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

member(Name)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.name==Name])),
    Member=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Member.

read(Key,Name)->
    Return=case read(Name) of
	       []->
		   {error,[eexist,Name,?FUNCTION_NAME,?MODULE,?LINE]};
	       {Name,Vsn,GitPath,Cmd} ->
		   case  Key of
		       name->
			   {ok,Name};
		       vsn->
			   {ok,Vsn};
		       gitpath->
			   {ok,GitPath};
		       cmd->
			   {ok,Cmd};
		       Err ->
			   {error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Return.

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{Name,Vsn,GitPath,Cmd}||{?RECORD,Name,Vsn,GitPath,Cmd}<-Z].

read(Object)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.name==Object])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{Name,Vsn,GitPath,Cmd}||{?RECORD,Name,Vsn,GitPath,Cmd}<-Z],
		   Info
	   end,
    Result.

delete(Object) ->
    F = fun() -> 
		mnesia:delete({?TABLE,Object})
		    
	end,
    mnesia:transaction(F).


do(Q) ->
    F = fun() -> qlc:e(Q) end,
    Result=case mnesia:transaction(F) of
	       {atomic, Val} ->
		   Val;
	       {error,Reason}->
		   {error,Reason}
	   end,
    Result.

%%-------------------------------------------------------------------------
init_table(SourceNode,DestNode)->
 %   ok=rpc:call(DestNode,?MODULE,create_table,[[DestNode]],20*1000),
    AllFileNames=rpc:call(SourceNode,config,application_all_filenames,[]),
    init_table(AllFileNames,SourceNode,DestNode).
    
init_table([],_,_)->
    ok;
init_table([FileName|T],SourceNode,DestNode)->
    {atomic,ok}=rpc:call(DestNode,?MODULE,create,
			 [FileName,
			  rpc:call(SourceNode,config,application_vsn,[FileName]),
			  rpc:call(SourceNode,config,application_gitpath,[FileName]),
			  rpc:call(SourceNode,config,application_start_cmd,[FileName])
			 ],20*1000),
    
    init_table(T,SourceNode,DestNode).
