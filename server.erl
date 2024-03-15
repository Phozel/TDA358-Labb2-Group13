-module(server).
-export([start/1,stop/1,server_loop/2, initial_channel_state/2, initial_server_state/2]).

-record(server_st, {
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
start(ServerAtom) ->
    catch genserver:start(ServerAtom, initial_server_state(ServerAtom, []), fun server_loop/2).

% Method to join a channel
server_loop(ServerSt, {join, ChannelAtom, Nick, ClientID}) ->
    %Check if channel already exists or not
    case lists:member(ChannelAtom, ServerSt#server_st.channel_list) of
        true ->
            %If Channel exists, forward the request to join, to the channel, return what the channel returns(either ok or already in channel)
            Replymsg = catch genserver:request(ChannelAtom, {join, Nick, ClientID}),
            {reply, Replymsg, ServerSt};
                
        false ->
            %If Channel doesn't exist, create a new channel with the client already as a member
            ChannelState = initial_channel_state(ChannelAtom, [[Nick, ClientID]]),
            catch genserver:start(ChannelAtom, ChannelState, fun channel/2),

            %Add the channel to the server's internal log over channels
            NewList = lists:append(ServerSt#server_st.channel_list, [ChannelAtom]),
            NewServerSt = ServerSt#server_st{channel_list = NewList},

            {reply, ok, NewServerSt}
    end;

% Method to stop the server and its channels
server_loop(ServerSt, stop) ->
    %Go through the server's internal list of channels and stop them one by one
    lists:foreach(fun(ChannelAtom) -> 
        catch genserver:stop(ChannelAtom) end, ServerSt#server_st.channel_list),
    {reply, ok, ServerSt#server_st{channel_list = []}}.
 
% Channel function to handle calls to join
channel(ChannelSt, {join, Nick, ClientID}) ->
    %Check if client is a member of the channel
    case lists:member([Nick, ClientID], ChannelSt#channel_st.client_list) of
        true ->
            %if yes, return appropriate response
            {reply, alreadyInChannel, ChannelSt};
        false ->
            %if no, add client to the channel's client_list, update channel state
            NewList = lists:append(ChannelSt#channel_st.client_list, [[Nick,ClientID]]),
            NewChannelSt = ChannelSt#channel_st{client_list = NewList},
            {reply, ok, NewChannelSt}
    end;

% Channel function to handle calls to leave
channel(ChannelSt, {leave, Nick, ClientID}) ->
    %Check if client is a member of the channel
    case lists:member([Nick, ClientID], ChannelSt#channel_st.client_list) of
        true ->
            %if yes, remove the client from the channel's client_list and return the updated channel state
            NewList = lists:delete([Nick, ClientID], ChannelSt#channel_st.client_list),
            NewChannelSt = ChannelSt#channel_st{client_list = NewList},
            {reply, ok, NewChannelSt};
        false ->
            %if no, return appropriate response
            {reply, channel_doesnt_have_client, ChannelSt}
    end;

% Channel function to handle calls to send messages
channel(ChannelSt, {message_send, Nick, Message, ClientID}) ->
    %Check whether or not client is a member of the channel
    case lists:member([Nick, ClientID], ChannelSt#channel_st.client_list) of
        true -> 
            %if yes;
            %We create a list without the sender so that they don't message themselves.
            OtherClients = lists:delete([Nick, ClientID], ChannelSt#channel_st.client_list),
            
            % A process is spawned that forwards the message to the other clients in the channel
            spawn(fun() -> 
                %Each Client in the channel is sent the message
                lists:foreach(fun(ClientInfo) ->
                    %Getting the clientID's from the newly created list of other members in the channel
                    CID = lists:nth(2,ClientInfo), 
                    catch genserver:request(CID, {message_receive, atom_to_list(ChannelSt#channel_st.channel), Nick, Message})
                end, 
            OtherClients) end),
            {reply, ok, ChannelSt};
        false ->
            %if no, return appropriate response
            {reply, user_not_joined, ChannelSt}
    end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    catch genserver:request(ServerAtom, stop),
    % Finally, we stop the server
    catch genserver:stop(ServerAtom),
    {reply, ok}.
