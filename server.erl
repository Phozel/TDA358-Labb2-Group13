-module(server).
-export([start/1,stop/1,server_loop/2, initial_channel_state/2, initial_server_state/3]).

% erlang:process_info(self(), messages) (page 15 föreläsning 7).

-record(server_st, { % kopierat från client.erl
    server_atom, % atom of server
    gui, % atom of the GUI process
    channel_list % list of channels in server
}).

-record(channel_st, {
    channel, % atom of channel
    client_list % list of active clients in channel
}).

initial_server_state(ServerAtom, GUIAtom, ChannelList)->
    #server_st{
        server_atom = ServerAtom,
        gui = GUIAtom,
        channel_list = ChannelList
    }.

initial_channel_state(ChannelAtom, Client_list) ->
    #channel_st{
        channel = ChannelAtom,
        client_list = Client_list
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    % genserver:start(ServerAtom, [],fun InsertMetod som "handles it" här),
    genserver:start(ServerAtom, [], server_loop).
    % sen kan vi pattern matcha för att göra olika saker beroende på vad vi får

% Main server loop function
% 
server_loop(ServerAtom, Request) ->
    case Request of
        {join, Channel} -> 
            member(Channel, channel_list),
            {reply, channel};
        %join channel
        _ ->     
            spawn(?MODULE, channel, []),
    
    io:fwrite("got to server_loop for join ~n", []),
    {reply, channel};

server_loop(ServerAtom, {Request_msg, Channel}) ->
    %case Channel of 
    server_loop(ServerAtom, Request_msg);

server_loop(ServerAtom, Request_msg) ->
    server_loop(ServerAtom, Request_msg).

channel(ChannelAtom, Client_list) ->
    %{message_receive, Channel, Nick, Msg}, byt ut Channel mot self() om det inte är så att man ska skicka tillbaka atomen den är kopplad till
    not_implemented.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.

%ping() -> 
%    io:fwrite("got to ping ~n", []),
%    receive
%        From -> io:fwrite("Ping! ~p~n", [From]),
%        ping();
%        _ -> ignore
%    after 10000 -> timeout
%    end.

