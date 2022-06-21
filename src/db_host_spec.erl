-module(db_host_spec).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,host_spec).
-define(RECORD,host_spec).
-record(host_spec,{
		   hostname,
		   local_ip,
		   public_ip,
		   ssh_port,
		   uid,
		   passwd,
		   application_config
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
init_table()->
    ok=create_table(),
    AllHostNames=config:host_all_hostnames(),
    init_table(AllHostNames).
    
init_table([])->
    ok;
init_table([HostName|T])->
    {atomic,ok}=create(HostName,
		       config:host_local_ip(HostName),
		       config:host_public_ip(HostName),
		       config:host_ssh_port(HostName),
		       config:host_uid(HostName),
		       config:host_passwd(HostName),
		       config:host_application_config(HostName)
		      ),
    
    init_table(T).
