%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  1
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_nodes).   
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([get_nodenames/0,
	 get_nodes/0,
	start_slave/1,
	 start_slave/2,
	start_nodes/0
	]). 


%% ====================================================================
%% External functions
%% ====================================================================


%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
%-define(NodeNames,["controller_0","controller_1",
%		   "controller_2","controller_3","controller_4"]).
-define(NodeNames,["c100","c200","c201","c202","c300"]).
get_nodenames()->
    ?NodeNames.    
get_nodes()->
    HostId=net_adm:localhost(),
    [list_to_atom(NodeName++"@"++HostId)||NodeName<-?NodeNames].
  %  [NodeName0,NodeName1,NodeName2]=?NodeNames,
  %  Node0=list_to_atom(NodeName0++"@"++HostId),
  %  Node1=list_to_atom(NodeName1++"@"++HostId),
  %  Node2=list_to_atom(NodeName2++"@"++HostId),
  %  [Node0,Node1,Node2].
    
start_slave(NodeName)->
    HostId=net_adm:localhost(),
    Node=list_to_atom(NodeName++"@"++HostId),
    rpc:call(Node,init,stop,[]),
    Cookie=atom_to_list(erlang:get_cookie()),
    Args="-pa ebin -setcookie "++Cookie,
    slave:start(HostId,NodeName,Args).


start_slave(NodeName,Pargs)->
    HostId=net_adm:localhost(),
    Node=list_to_atom(NodeName++"@"++HostId),
    rpc:call(Node,init,stop,[]),
    Cookie=atom_to_list(erlang:get_cookie()),
    Args=Pargs++" "++"-setcookie "++Cookie,
    slave:start(HostId,NodeName,Args).
    
start_nodes()->

    [rpc:call(N,init,stop,[],1*1000)||N<-get_nodes()],
    timer:sleep(2000),
  
    [start_slave(NodeName)||NodeName<-?NodeNames],
   % gl=atom_to_list(erlang:get_cookie()), 
    [net_adm:ping(N)||N<-get_nodes()],
    ok.
