-module(server).
-export([loop/2, initial_state/1]).
-import(lists, [member/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Pid}) -> 
	case member(_Pid, St#server_st.userpids) of
		true ->  {not_ok, St};
		false -> St2 = St#server_st{userpids = St#server_st.userpids++[_Pid]},
    	{ok, St2}
	end;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%	
loop(St, {disconnect, _Pid}) -> 
	case member(_Pid, St#server_st.userpids) of
	false -> {not_ok, St};
	true ->  St2 = St#server_st{userpids = St#server_st.userpids--[_Pid]},
    {ok, St2}
	end;

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St, {join, _Channel, _Nick, _CLPID}) ->
	case member(_Channel, St#server_st.channels) of
		true -> 
			Join = genserver:request(list_to_atom(_Channel), {join, list_to_atom(_Nick), _CLPID}),
			case Join of
				error 	-> {error, St};
				ok 		-> {ok,St}
			end;
		false -> 
		genserver:start(list_to_atom(_Channel), channel:initial_state(_Channel), 
                    fun channel:loop/2),
		St2 = St#server_st{channels = St#server_st.channels++[_Channel]},
		genserver:request(list_to_atom(_Channel), {join, list_to_atom(_Nick), _CLPID}),
		{ok, St2}
	end;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel, _Nick, _CLPID}) ->
	case member(_Channel, St#server_st.channels) of
		true -> 
		Leave = genserver:request(list_to_atom(_Channel), {leave, list_to_atom(_Nick), _CLPID}),
		case Leave of
			ok -> {ok,St};
			error -> {error, St}
		end;
		false -> {error, St} 
	end;	

%%%%%%%%%%%%%%%
%%%% Messages
%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg, _Nick, _CLPID}) -> 
	case member(_Channel, St#server_st.channels) of
		true ->	genserver:request(list_to_atom(_Channel),{msg_from_GUI, _Msg, _Nick, _CLPID}),
		{ok,St};
		false -> {error, St} 
	end.	

%%%%%%%%%%%%%%%%%%%%%
%%%% Initial state
%%%%%%%%%%%%%%%%%%%%%
initial_state(_Server) ->
    #server_st{server = _Server, userpids = [], channels = []}.