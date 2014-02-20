% This record defines the structure of the 
% client process. 
% 
% It contains the following fields: 
%
% gui: it stores the name (or Pid) of the GUI process.
%
-record(cl_st, {gui,nick,server}).
    
% This record defines the structure of the 
% server process. 
% 
-record(server_st, {server, users, channels}).




-record(channel_st, {channel, users}).
