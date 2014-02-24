-module(client).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Server}) ->
    Con = genserver:request(list_to_atom(_Server), {connect, self()}),
	case Con of 
		not_ok -> {{error, user_not_connected, "User already connected"}, St};
		ok -> St2 = St#cl_st{server = _Server}, {ok, St2}
	end;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
loop(St, disconnect) ->
	Server = St#cl_st.server,
	case Server of
	"" -> {{error, user_not_connected, "YOU ARE NOT EVEN CONNECTED"}, St};
	_ -> 
		Dis = genserver:request(list_to_atom(St#cl_st.server), {disconnect, self()}),
		case Dis of 
			not_ok -> {{error, user_not_connected, "YOU ARE NOT EVEN CONNECTED"}, St};
			ok -> St2 = St#cl_st{server = ""}, {ok, St2}
		end
	end;

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join,_Channel}) ->
	Server = St#cl_st.server,
	case Server of
	"" -> {{error, user_not_connected, "YOU ARE NOT EVEN CONNECTED"}, St};
	_ -> 
		Join = genserver:request(list_to_atom(St#cl_st.server), {join, _Channel, St#cl_st.nick, self()}),
		case Join of 
			error -> {{error, user_already_connected, "User already connected to channel"}, St};
			ok -> {ok, St}
		end
	end;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel}) ->
 	Server = St#cl_st.server,
	case Server of
	"" -> {{error, user_not_connected, "YOU ARE NOT EVEN CONNECTED"}, St};
	_ -> 
		Leave = genserver:request(list_to_atom(St#cl_st.server), {leave, _Channel, St#cl_st.nick, self()}),
		case Leave of 
			error -> {{error, user_not_a_member, "User is not a member of this channel"}, St};
			ok -> {ok, St}
		end
	end;    

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
     Message = genserver:request(list_to_atom(St#cl_st.server),{msg_from_GUI,_Channel,_Msg,St#cl_st.nick , self()}),
     case Message of
     	ok -> {ok, St};
     	error -> {{error, no_such_channel, "Channel does not exist"}, St}
     end;


%%%%%%%%%%%%%%
%%% WhoIam
%%%%%%%%%%%%%%
loop(St, whoiam) ->
	{St#cl_st.nick, St};

%%%%%%%%%%
%%% Nick
%%%%%%%%%%
loop(St,{nick,_Nick}) ->
    {ok, St#cl_st{ nick = _Nick }};

%%%%%%%%%%%%%
%%% Debug
%%%%%%%%%%%%%
loop(St, debug) ->
    {St, St};

%%%%%%%%%%%%%%%%%%%%%
%%%% Incoming message
%%%%%%%%%%%%%%%%%%%%%
% What does _MsgFromClient want as an input?
%loop(St = #cl_st { gui = GUIName }, _MsgFromClient) ->
 %   {Channel, Name, Msg} = decompose_msg(_MsgFromClient),
  %  gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
   % {ok, St}.

loop(St = #cl_st { gui = GUIName }, {_Channel, _Name, _Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, _Channel, _Name++"> "++_Msg}),
    {ok, St}.


% This function will take a message from the client and
% decomposed in the parts needed to tell the GUI to display
% it in the right chat room.
decompose_msg(_MsgFromClient) ->
    {"", "", ""}.


%%%%%%%%%%%%%%%%%%%%%
%%%% Initial state
%%%%%%%%%%%%%%%%%%%%%
initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, server = "" }.
