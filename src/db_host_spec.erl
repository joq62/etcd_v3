-module(db_host_spec).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_host_spec.hrl").

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
		   mnesia:add_table_copy(schema, node(),StorageType),
		   mnesia:add_table_copy(?TABLE, node(), StorageType),
		   Tables=mnesia:system_info(tables),
		   mnesia:wait_for_tables(Tables,20*1000);
	       Reason ->
		   Reason
	   end,
    Result.

create(HostName,LocalIp,PublicIp,SshPort,Uid,Passwd,ApplicationConfig)->
    Record=#?RECORD{
		    hostname=HostName,
		    local_ip=LocalIp,
		    public_ip=PublicIp,
		    ssh_port=SshPort,
		    uid=Uid,
		    passwd=Passwd,
		    application_config=ApplicationConfig
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

member(HostName)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.hostname==HostName])),
    Member=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Member.

read(Key,HostName)->
    Return=case read(HostName) of
	       []->
		   {error,[eexist,HostName,?FUNCTION_NAME,?MODULE,?LINE]};
	       {_HostName,LocalIp,PublicIp,SshPort,Uid,Passwd,ApplicationConfig} ->
		   case  Key of
		       local_ip->
			   {ok,LocalIp};
		       public_ip->
			   {ok,PublicIp};
		       ssh_port->
			   {ok,SshPort};
		       uid->
			   {ok,Uid};
		       passwd->
			   {ok,Passwd};
		       application_config->
			   {ok,ApplicationConfig};
		       Err ->
			   {error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Return.


get_all_hostnames()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [HostName||{?RECORD,HostName,_LocalIp,_PublicIp,_SshPort,_Uid,_Passwd,_ApplicationConfig}<-Z].
    
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{HostName,LocalIp,PublicIp,SshPort,Uid,Passwd,ApplicationConfig}||{?RECORD,HostName,LocalIp,PublicIp,SshPort,Uid,Passwd,ApplicationConfig}<-Z].

read(Object)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.hostname==Object])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{HostName,LocalIp,PublicIp,SshPort,Uid,Passwd,ApplicationConfig}||{?RECORD,HostName,LocalIp,PublicIp,SshPort,Uid,Passwd,ApplicationConfig}<-Z],
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
 %   ok=rpc:call(DestNode,?MODULE,create_table,[[DestNode]]),
    AllHostNames=rpc:call(SourceNode,config,host_all_hostnames,[]),
    init_table(AllHostNames,SourceNode,DestNode).
    
init_table([],_,_)->
    ok;
init_table([HostName|T],SourceNode,DestNode)->
    {atomic,ok}=rpc:call(DestNode,?MODULE,create,
			 [HostName,
			  rpc:call(SourceNode,config,host_local_ip,[HostName]),
			  rpc:call(SourceNode,config,host_public_ip,[HostName]),
			  rpc:call(SourceNode,config,host_ssh_port,[HostName]),
			  rpc:call(SourceNode,config,host_uid,[HostName]),
			  rpc:call(SourceNode,config,host_passwd,[HostName]),
			  rpc:call(SourceNode,config,host_application_config,[HostName])
			 ]),
    
    init_table(T,SourceNode,DestNode).
