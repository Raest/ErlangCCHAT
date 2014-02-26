-module(server).
-export([loop/2, initial_state/1]).
-import(lists, [member/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Nick}) -> 
	case member(_Nick, St#server_st.users) of
		true ->  {not_ok, St};
		false -> St2 = St#server_st{users = St#server_st.users++[_Nick]},
    	{ok, St2}
	end;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%	
loop(St, {disconnect, _Nick}) -> 
	case member(_Nick, St#server_st.users) of
	false -> {not_ok, St};
	true ->  St2 = St#server_st{users = St#server_st.users--[_Nick]},
    {ok, St2}
	end;

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St, {join, _Channel, _Pid}) ->
	case member(_Channel, St#server_st.channels) of
		true -> 
			Join = genserver:request(list_to_atom(_Channel), {join, _Pid}),
			case Join of
				error 	-> {error, St};
				ok 		-> {ok,St}
		end;
		false -> 
			genserver:start(list_to_atom(_Channel), channel:initial_state(_Channel), 
                    fun channel:loop/2),
			St2 = St#server_st{channels = St#server_st.channels++[_Channel]},
			genserver:request(list_to_atom(_Channel), {join, _Pid}),
			{ok, St2}
		end;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel, _Pid}) ->
	case member(_Channel, St#server_st.channels) of
		true -> 
		Leave = genserver:request(list_to_atom(_Channel), {leave, _Pid}),
		case Leave of
			ok -> {ok,St};
			error -> {error, St}
		end;
		false -> {error, St} 
	end;	

%%%%%%%%%%%%%%%
%%%% Messages
%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg, _Nick, _Pid}) -> 
	case member(_Channel, St#server_st.channels) of
		true ->	genserver:request(list_to_atom(_Channel),{_Msg, _Nick, _Pid}),
		{ok,St};
		false -> {error, St} 
	end.	

%%%%%%%%%%%%%%%%%%%%%
%%%% Initial state
%%%%%%%%%%%%%%%%%%%%%
initial_state(_Server) ->
    #server_st{server = _Server, users = [], channels = []}.