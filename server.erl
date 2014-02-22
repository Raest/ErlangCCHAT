-module(server).
-export([loop/2, initial_state/1]).
-import(lists, [member/2]).
-import(erl, [display/1]).

-include_lib("./defs.hrl").

loop(St, {connect, _Nick}) -> 
	case member(_Nick, St#server_st.users) of
		true -> {not_ok, St};
		false -> St2 = St#server_st{users=St#server_st.users++[_Nick]},
    	{ok, St2}
	end;
	
loop(St, {disconnect, _Nick}) -> 
	case member(_Nick, St#server_st.users) of
	false -> 
	{not_ok, St};
	true ->
	St2 = St#server_st{users=St#server_st.users--[_Nick]},
    {ok, St2}
	end;

loop(St, {join, _Channel, _Nick}) ->
	case member(_Channel, St#server_st.channels) of
		true -> genserver:request(list_to_atom(_Channel), {join, list_to_atom(_Nick)}),
				{ok,St};
		false -> 
		genserver:start(list_to_atom(_Channel), channel:initial_state(_Channel), 
                    fun channel:loop/2),
		St2 = St#server_st{channels = St#server_st.channels++[_Channel]},
		genserver:request(list_to_atom(_Channel), {join, list_to_atom(_Nick)}),
		{ok, St2}
	end;

loop(St, {leave, _Channel, _Nick}) ->
	case member(_Channel, St#server_st.channels) of
		true -> genserver:request(list_to_atom(_Channel), {leave, list_to_atom(_Nick)}),
				{ok,St};
		false -> {{error, user_not_connected, "Channel does not exist"}, St} 
	end;	

loop(St, {message, _Channel, _Msg}) -> 
	case member(_Channel, St#server_st.channels) of
		true ->	genserver:request(list_to_atom(_Channel),{message, _Msg}),
		{ok,St};
		false -> {{error, user_not_connected, "Channel does not exist"}, St} 
	end.	

initial_state(_Server) ->
    #server_st{server = _Server, users = [], channels = []}.