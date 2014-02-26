-module(channel).
-export([loop/2, initial_state/1]).
-import(lists, [member/2]).
-import(erl, [display/1, foreach/2, to_atom/1]).
-include_lib("./defs.hrl").

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join, _Pid}) ->
	case member(_Pid, St#channel_st.userpids) of 
		true 	->			io:format("redan med",[]),
		{error, St};
		false	->	St2 = St#channel_st{userpids = St#channel_st.userpids++[_Pid]}, 
					{ok,St2} 
	end;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St,{leave, _Pid})->
	case member(_Pid, St#channel_st.userpids) of 
		true	->	St2 = St#channel_st{userpids = St#channel_st.userpids--[_Pid]}, 
					{ok,St2};
		false 	->	{error, St} 
	end;

%%%%%%%%%%%%%%%
%%%% Messages
%%%%%%%%%%%%%%%
loop(St,{_Msg, _Nick, _Pid}) ->
	Chan = St#channel_st.channel,
	List = [ Pid || Pid <- St#channel_st.userpids, Pid =/= _Pid],
	iterate(List, {Chan, _Nick, _Msg}),
	{ok, St}.
	
iterate([], _Msg) -> ok;
iterate([H|T], _Msg) ->
	genserver:request(H, _Msg),
	iterate(T, _Msg).

%%%%%%%%%%%%%%%%%%%%%
%%%% Initial state
%%%%%%%%%%%%%%%%%%%%%
initial_state(_Channel) ->
    #channel_st{channel = _Channel, userpids = []}.