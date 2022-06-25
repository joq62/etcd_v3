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
-module(etcd_eunit).   
 
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
    
   
    ok=start_node_etcd(),
    io:format("sd:all() ~p~n",[sd:all()]),

    ok=db_host_spec:init_table(),
   "192.168.1.202"=config:host_local_ip("c202"),
    io:format("db_host_spec:read_all() ~p~n",[db_host_spec:read_all()]),

    ok=db_application_spec:init_table(),
    "https://github.com/joq62/nodelog.git"=config:application_gitpath("nodelog.spec"),
    io:format("db_application_spec:read_all() ~p~n",[db_application_spec:read_all()]),

    ok=db_deployment_info:init_table(),
    "calculator"=config:deployment_name("calculator.depl"),
    io:format("db_deployment_info:read_all() ~p~n",[db_deployment_info:read_all()]),

    ok=db_deployments:init_table(),
    "cluster1_cookie"=config:deployment_spec_cookie("cluster1.depl_spec"),
    io:format("db_deployments:read_all() ~p~n",[db_deployments:read_all()]),


   % init:stop(),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
check_application_spec()->
  
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
check_host_spec()->
    HostName="c202",
    "192.168.1.202"=config:host_local_ip("c202"),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
init_host_spec()->
    ok=db_host_spec:create_table(),
    AllHostNames=config:host_all_hostnames(),
    init_host_spec(AllHostNames).
    
init_host_spec([])->
    ok;
init_host_spec([HostName|T])->
    {atomic,ok}=db_host_spec:create(HostName,
				    config:host_local_ip(HostName),
				    config:host_public_ip(HostName),
				    config:host_ssh_port(HostName),
				    config:host_uid(HostName),
				    config:host_passwd(HostName),
				    config:host_application_config(HostName)
				   ),
    
    init_host_spec(T).


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start_node_etcd()->
    ok=sd:appl_start([]),
    pong=sd:ping(),
    ok=config:appl_start([]),
    pong=config:ping(),
    ok=etcd:appl_start([]),
    pong=etcd:ping(), 
    ok=etcd:dynamic_db_init([]),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
  
    % Simulate host
  %  R=rpc:call(node(),test_nodes,start_nodes,[],2000),
%    [Vm1|_]=test_nodes:get_nodes(),

%    Ebin="ebin",
 %   true=rpc:call(Vm1,code,add_path,[Ebin],5000),
    R=ok,
    R.
