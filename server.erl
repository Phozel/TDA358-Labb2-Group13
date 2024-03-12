-module(server).
-export([start/1,stop/1,server_loop/2, initial_channel_state/2, initial_server_state/2]).

% erlang:process_info(self(), messages) (page 15 föreläsning 7).

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
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    % genserver:start(ServerAtom, [],fun InsertMetod som "handles it" här),
    
    catch genserver:start(ServerAtom, initial_server_state(ServerAtom, []), fun server_loop/2).

% Main server loop function
% Maybe remove ServerAtom?
server_loop(ServerSt, {Request, ChannelAtom, Nick, ClientID, Msg}) ->
    case Request of
        join -> 
            case lists:member(ChannelAtom, ServerSt#server_st.channel_list) of
                true ->
                    Replymsg = genserver:request(ChannelAtom, {join, Nick, ClientID}),
                    {reply, Replymsg, ServerSt};
                        
                false ->
                    ChannelState = initial_channel_state(ChannelAtom, [[Nick, ClientID]]),
                    catch genserver:start(ChannelAtom, ChannelState, fun channel/2),

                    NewList = lists:append(ServerSt#server_st.channel_list, [ChannelAtom]),
                    NewServerSt = ServerSt#server_st{channel_list = NewList},

                    {reply, ok, NewServerSt}
            end;
        
        leave ->
            Reply = catch genserver:request(ChannelAtom, {leave, Nick, ClientID}),
            {reply, Reply, ServerSt};
        
        message_send ->
            Reply = catch genserver:request(ChannelAtom, {message_send, Nick, Msg, ClientID}),
            {reply, Reply, ServerSt}
            

    end.
 
% Want to reach the channel's client_list somehow, maybe we can't have the client list as a parameter
channel(ChannelSt, {join, Nick, ClientID}) ->
    case lists:member([Nick, ClientID], ChannelSt#channel_st.client_list) of
        true ->
            {reply, alreadyInChannel, ChannelSt};
        false ->
            NewList = lists:append(ChannelSt#channel_st.client_list, [[Nick,ClientID]]),
            NewChannelSt = ChannelSt#channel_st{client_list = NewList},
            {reply, ok, NewChannelSt}
    end;

channel(ChannelSt, {leave, Nick, ClientID}) ->
    case lists:member([Nick, ClientID], ChannelSt#channel_st.client_list) of
        true ->
            NewList = lists:delete([Nick, ClientID], ChannelSt#channel_st.client_list),
            NewChannelSt = ChannelSt#channel_st{client_list = NewList},
            {reply, ok, NewChannelSt};
        false ->
            {reply, channel_doesnt_have_client, ChannelSt}
    end;

channel(ChannelSt, {message_send, Nick, Message, ClientID}) ->
    case lists:member([Nick, ClientID], ChannelSt#channel_st.client_list) of
        true -> 
            %We create a list without the sender so that they don't message themselves.
            OtherClients = lists:delete([Nick, ClientID], ChannelSt#channel_st.client_list),
            
            %
            spawn(fun()->lists:foreach(
                fun(ClientInfo) ->
                    X = lists:nth(2,ClientInfo), 
                    if ClientID == X -> 
                        skip;
                    true ->
                        genserver:request(X, {message_receive, atom_to_list(ChannelSt#channel_st.channel), Nick, Message})
                    end
                end,
            OtherClients) end),
            {reply, ok, ChannelSt};
        false ->
            {reply, user_not_joined, ChannelSt}
    end.

% recSendMsg(ChannelSt, Msg, ClientList) ->
%     case ClientList of 
%         [] -> 
%             ok;
%         [[CurrNick, CurrID] | OtherClients] ->
%             CurrID ! {request, CurrID, make_ref(), {message_receive, ChannelSt#channel_st.channel, CurrNick, Msg}},
%             recSendMsg(ChannelSt, Msg, OtherClients)
%     end.
    
    

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    lists:foreach(genserver:stop(), ServerAtom#server_st.channel_list),
    genserver:stop(ServerAtom),
    {reply, ok}.

