-module(channel).
-export([loop/2, initial_state/1]).
-import(lists, [member/2]).
-import(erl, [display/1, foreach/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join, _Nick, _CLPID}) ->
	case member(_Nick, St#channel_st.users) of 
		true 	->	{error, St};
		false	->	St2 = St#channel_st{users = St#channel_st.users++[_Nick], 
					userpids = St#channel_st.userpids++[_CLPID]}, 
					{ok,St2} 
	end;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St,{leave, _Nick, _CLPID})->
	case member(_Nick, St#channel_st.users) of 
		true	->	St2 = St#channel_st{users = St#channel_st.users--[_Nick], 
					userpids = St#channel_st.userpids--[_CLPID]}, 
					{ok,St2};
		false 	->	{error, St} 
	end;

%%%%%%%%%%%%%%%
%%%% Messages
%%%%%%%%%%%%%%%
loop(St,{msg_from_GUI, _Msg, _Nick, _CLPID}) ->
	Chan = #channel_st.channel,
	Clpid = _CLPID,
	[Pid ! {self(), {Chan, _Nick,_Msg}}|| Pid <- St#channel_st.userpids, Pid =/= Clpid],
	%[genserver:request(list_to_atom(self()), {Chan, _Nick,_Msg}) || Pid <- St#channel_st.userpids, Pid =/= Clpid],
	{ok, St}.


%loop(St,{msg_from_GUI, _Msg}) ->
%	iterate(St#channel_st.users, St, _Msg).
%iterate([], St, _Msg) -> {ok, St};
%iterate([H|T], St, _Msg) ->
%	genserver:request(list_to_atom(H), _Msg),
%	iterate(T, St, _Msg).

%%%%%%%%%%%%%%%%%%%%%
%%%% Initial state
%%%%%%%%%%%%%%%%%%%%%
initial_state(_Channel) ->
    #channel_st{channel = _Channel, users = [], userpids = []}.