-module(server).
-export([start/1,stop/1,server_loop/2, initial_channel_state/2, initial_server_state/2]).

% erlang:process_info(self(), messages) (page 15 föreläsning 7).

-record(server_st, { % kopierat från client.erl
    server_atom, % atom of server
    channel_list, % list of channels in server
    channel_dict
}).

-record(channel_st, {
    channel, % atom of channel
    client_list % list of active clients in channel
}).

initial_server_state(ServerAtom, ChannelList)->
    #server_st{
        server_atom = ServerAtom,
        channel_list = ChannelList,
        channel_dict = []
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
    
    catch genserver:start(ServerAtom, initial_server_state(ServerAtom, []), fun server_loop/2).

    % sen kan vi pattern matcha för att göra olika saker beroende på vad vi får

% Main server loop function
% Maybe remove ServerAtom?
server_loop(ServerSt, {Request, ChannelAtom, Nick, Msg}) ->
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
            io:fwrite("Nick: ~p~n", [Nick]),
            %io:fwrite("ResultChannelExists: ~p~n", [isExistingChannel(list_to_atom(ChannelAtom), ServerSt#server_st.channel_list)]),
            case lists:member(ChannelAtom, ServerSt#server_st.channel_list) of % ServerSt#server_st.channel_list#channel_rec.channel_list
                true ->
                    io:fwrite("got to true ~n", []),
                    Reply = genserver:request(ChannelAtom, {join, Nick}),
                    io:fwrite("got past request ~n", []),
                    %check if isMember, if so join. Else 
                    case Reply of 
                        welcome -> 
                            {reply, ok, ChannelAtom};
                        alreadyInChannel -> 
                            {reply, wasInChannel, ChannelAtom}
                    end;
                    % ResultClientIsMember = isMember(ChannelAtom)
                        
                false ->
                    io:fwrite("got to false ~n", []),
                    ChannelState = initial_channel_state(ChannelAtom, []),
                    %catch genserver:start(list_to_atom(ChannelAtom), #channel_st{channel = ChannelAtom, client_list = []}, fun channel/2),
                    catch genserver:start(ChannelAtom, ChannelState, fun channel/2),
                    %io:fwrite("Channel List prior to adding channel to server: ~p~n", [ServerSt#server_st.channel_list]),

                    NewList = lists:append(ServerSt#server_st.channel_list, [ChannelAtom]),
                    NewDict = lists:append(ServerSt#server_st.channel_dict, [{ChannelAtom, ChannelState}]),
                    NewServerSt = ServerSt#server_st{channel_list = NewList, channel_dict = NewDict},
                
                    %io:fwrite("list operation: ~p~n", [lists:append(ServerSt#server_st.channel_list, [ChannelAtom])]),               
                    io:fwrite("Channel List after adding chaServerSt#server_st.nnel to server: ~p~n", [NewList]),
                    Reply = catch genserver:request(ChannelState#channel_st.channel, {join, Nick, ChannelState}),
                    io:fwrite("Reply is: ~p~n", [Reply]),
                    ChannelSt = getChannelState(ChannelAtom, NewServerSt#server_st.channel_dict),
                    io:fwrite("ChannelSt: ~p~n", [ChannelSt]),
                    %{reply, Reply, ChanAtom} %får se om det går att skicka tillbaka detta
                    {reply, Reply, NewServerSt}
            end;
            %io:fwrite("got to server_loop for join ~n", [])
        leave ->
            %Reply = catch genserver:request(ChannelAtom, {leave, Nick, ChannelState}),
            %{reply, Reply, ServerSt},
            not_implemented;
        
        message_send ->
            %Reply = catch genserver:request(ChannelState#channel_st.channel, {message_send, Nick, Msg, ChannelState}),
            
            not_implemented

    end.

    
% Want to reach the channel's client_list somehow, maybe we can't have the client list as a parameter
channel(ChannelAtom, {join, Nick, ChannelSt}) ->
    %{message_receive, Channel, Nick, Msg}, byt ut Channel mot self() om det inte är så att man ska skicka tillbaka atomen den är kopplad till
    io:fwrite("first channel ~n", []),
    case lists:member(Nick, ChannelSt#channel_st.client_list) of
        true ->
            {reply, {user_already_joined, ChannelAtom}, ChannelSt};
        false ->
            NewList = lists:append(ChannelSt#channel_st.client_list, [Nick]),
            NewChannelSt = ChannelSt#channel_st{client_list = NewList},
            io:fwrite("before replying: ~n", []),
            {reply, {ok, ChannelAtom}, NewChannelSt}
    end;
    

channel(ChannelAtom, {leave, Nick, ChannelSt}) ->
    case lists:member(Nick, ChannelSt#channel_st.client_list) of
        true ->
            NewList = lists:delete(Nick, ChannelSt#channel_st.client_list),
            NewChannelSt = ChannelSt#channel_st{client_list = NewList},
            {reply, {ok, ChannelAtom}, NewChannelSt};
        false ->
            {reply, {user_not_joined, ChannelAtom}, ChannelSt}
    end;


channel(ChannelAtom, {message_send, Nick, Message, ChannelSt}) ->
    io:fwrite("third channel ~n", []),
    case lists:member(Nick, ChannelSt#channel_st.client_list) of
        true -> 
            %send message and then reply

            {reply, {ok, ChannelAtom}, ChannelSt},
            not_implemented;
        false ->
            {reply, user_not_joined, ChannelAtom, ChannelSt}
    end.
    %not_implemented;

getChannelState(ChannelAtom, ServerDict) ->
    ChannelTuple = lists:keyfind(ChannelAtom, 1, ServerDict),
    case ChannelTuple of
        false ->
            not_implemented;
        _ ->
            {element(2, ChannelTuple)}
    end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.

