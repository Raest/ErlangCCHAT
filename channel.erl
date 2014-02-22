-module(channel).
-export([loop/2, initial_state/1]).
-import(lists, [member/2]).
-import(erl, [display/1]).

-include_lib("./defs.hrl").

loop(St,{join, _Nick}) ->
	case member(_Nick, St#channel_st.users) of 
		true 	->	{{error, user_not_connected, "User already in channel"}, St};
		false	->	St2 = St#channel_st{users = St#channel_st.users++[_Nick]}, 
					{ok,St2} 
	end;

loop(St,{leave, _Nick})->
	case member(_Nick, St#channel_st.users) of 
		true	->	St2 = St#channel_st{users = St#channel_st.users--[_Nick]}, 
					{ok,St2};
		false 	->	{{error, user_not_connected, "User not in channel"}, St} 
	end;

loop(St,{message, _Msg}) ->
	
	{ok,St}.

initial_state(_Channel) ->
    #channel_st{channel = _Channel, users = []}.