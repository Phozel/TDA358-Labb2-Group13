-module(server).
-export([start/1,stop/1,server_loop/2, initial_channel_state/2, initial_server_state/2]).

% erlang:process_info(self(), messages) (page 15 föreläsning 7).

-record(server_st, { % kopierat från client.erl
    server_atom, % atom of server
    channel_list % list of channels in server
}).

-record(channel_st, {
    channel, % atom of channel
    client_list % list of active clients in channel
}).

initial_server_state(ServerAtom, ChannelList)->
    #server_st{
        server_atom = ServerAtom,
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
    catch genserver:start(ServerAtom, [], fun server_loop/2).

    % sen kan vi pattern matcha för att göra olika saker beroende på vad vi får

% Main server loop function
% Maybe remove ServerAtom?
server_loop(ClientState, {Request, ChannelAtom, ServerSt}) ->
    io:fwrite("Got to serverloop ~n", []),
    case Request of
        % If /join is called
        % check if channel exists
        %    if YES
        %       is client member of channel?
        %            if NO, try to join channel
        %            else return msg that client already is member
        %    if NO
        %       create channel
        %       join the newly created channel
        join -> 
            io:fwrite("got to server_loop for join ~n", []),
            io:fwrite("ResultChannelExists: ~p~n", [isExistingChannel([ChannelAtom], ServerSt)]),
            case isExistingChannel([ChannelAtom], ServerSt) of
                true ->
                    io:fwrite("got to true ~n", []),
                    %genserver:request(ChannelAtom, reply),
                    %channel(ChannelAtom, {join,ClientState})
                    %check if isMember, if so join. Else 
                    {reply, ok, ClientState};
                    % ResultClientIsMember = isMember(ChannelAtom)
                        
                false ->
                    %initial_channel_state(ChannelAtom, []),
                    io:fwrite("got to false ~n", []),
                    catch genserver:start(list_to_atom(ChannelAtom), #channel_st{channel = ChannelAtom, client_list = []}, fun channel/2),
                    io:fwrite("Channel List prior to adding channel to server: ~p~n", [ServerSt#server_st.channel_list]),
                    ServerSt#server_st{channel_list = lists:append(ServerSt#server_st.channel_list, [ChannelAtom])},                    
                    io:fwrite("Channel List after to adding channel to server: ~p~n", [ServerSt#server_st.channel_list]),
                    server_loop(ClientState, {Request, ChannelAtom, ServerSt})
                    %{reply, ChannelAtom, ClientState}
            end
            
            
            %io:fwrite("got to server_loop for join ~n", [])
        end.

% server_loop(ServerAtom, {Request_msg, Channel}) ->
%     %case Channel of 
%     server_loop(ServerAtom, {Request_msg, Channel});

% server_loop(ServerAtom, Request_msg) ->
%     server_loop(ServerAtom, Request_msg).

isExistingChannel(Channel, ServerSt) ->
    Channels = ServerSt#server_st.channel_list,
    io:fwrite("Channel List: ~p~n", [Channels]),
    lists:member(Channel, Channels).

%isMember(Channel) ->
    %lists:members(ClientState, client_list).
    
channel(ChannelAtom, {join, {gui, nick, server}}) ->
    %{message_receive, Channel, Nick, Msg}, byt ut Channel mot self() om det inte är så att man ska skicka tillbaka atomen den är kopplad till
    % case lists:member(nick, client_list) of
    %     true ->
    %         not_implemented;
    %     false ->
    %         not_implemented
    % end.
    io:fwrite("first channel ~n", []),
    not_implemented;

channel(ChannelAtom, {leave, {gui, nick, server}}) ->
    io:fwrite("second channel ~n", []),
    %{message_receive, Channel, Nick, Msg}, byt ut Channel mot self() om det inte är så att man ska skicka tillbaka atomen den är kopplad till
    not_implemented;

channel(ChannelAtom, {_}) ->
    io:fwrite("third channel ~n", []),

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

