-module(channel).
-export([loop/2, initial_state/1]).
-import(lists, [member/2]).
-import(erl, [display/1, foreach/2]).

-include_lib("./defs.hrl").

loop(St,{join, _Nick}) ->
	case member(_Nick, St#channel_st.users) of 
		true 	->	{error, St};
		false	->	St2 = St#channel_st{users = St#channel_st.users++[_Nick]}, 
					{ok,St2} 
	end;

loop(St,{leave, _Nick})->
	case member(_Nick, St#channel_st.users) of 
		true	->	St2 = St#channel_st{users = St#channel_st.users--[_Nick]}, 
					{ok,St2};
		false 	->	{error, St} 
	end;

loop(St,{msg_from_GUI, _Msg}) ->
	{ok, St}.

%loop(St,{msg_from_GUI, _Msg}) ->
%	foreach(
%        fun(X) -> genserver:request(list_to_atom([X]), _Msg) end,
%        St#channel_st.users), {ok, St}.


%loop(St,{msg_from_GUI, _Msg}) ->
%	iterate(St#channel_st.users, St, _Msg).
%iterate([], St, _Msg) -> {ok, St};
%iterate([H|T], St, _Msg) ->
%	genserver:request(list_to_atom(H), _Msg),
%	iterate(T, St, _Msg).

initial_state(_Channel) ->
    #channel_st{channel = _Channel, users = []}.