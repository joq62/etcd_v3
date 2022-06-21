-module(db_deployments).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,deployments).
-define(RECORD,deployments).
-record(deployments,{
		     name,
		     controllers,
		     workers,
		     cookie,
		     hosts,
		     deployments
		    }).

 
create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

add_node(Node,StorageType)->
    Result=case mnesia:change_config(extra_db_nodes, [Node]) of
	       {ok,[Node]}->
		   mnesia:add_table_copy(schema, node(),StorageType),
		   mnesia:add_table_copy(?TABLE, node(), StorageType),
		   Tables=mnesia:system_info(tables),
		   mnesia:wait_for_tables(Tables,20*1000);
	       Reason ->
		   Reason
	   end,
    Result.

create(Name,Controllers,Workers,Cookie,Hosts,Deployments)->
    Record=#?RECORD{
		    name=Name,
		    controllers=Controllers,
		    workers=Workers,
		    cookie=Cookie,
		    hosts=Hosts,
		    deployments=Deployments
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
	       {Name,Controllers,Workers,Cookie,Hosts,Deployments} ->
		   case  Key of
		       name->
			   {ok,Name};
		       controllers->
			   {ok,Controllers};
		       workers->
			   {ok,Workers};
		       cookie->
			   {ok,Cookie};
		       hosts->
			   {ok,Hosts};
		       deployments->
			   {ok,Deployments};
		       Err ->
			   {error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Return.

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{Name,Controllers,Workers,Cookie,Hosts,Deployments}||{?RECORD,Name,Controllers,Workers,Cookie,Hosts,Deployments}<-Z].

read(Object)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.name==Object])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{Name,Controllers,Workers,Cookie,Hosts,Deployments}||{?RECORD,Name,Controllers,Workers,Cookie,Hosts,Deployments}<-Z],
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
init_table()->
    ok=create_table(),
    AllFileNames=config:deployment_spec_all_filenames(),
    init_table(AllFileNames).
    
init_table([])->
    ok;
init_table([FileName|T])->
    {atomic,ok}=create(
		  config:deployment_spec_name(FileName),
		  config:deployment_spec_controllers(FileName),
		  config:deployment_spec_workers(FileName),
		  config:deployment_spec_cookie(FileName),
		  config:deployment_spec_hosts(FileName),
		  config:deployment_spec_deployments(FileName)
		 ),
    
    init_table(T).
