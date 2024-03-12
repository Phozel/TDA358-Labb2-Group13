-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channel_list
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channel_list = []
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    %TODO: Check if the server exists/responds
    case lists:member(Channel, St#client_st.channel_list) of
        true -> 
            {reply, {error, user_already_joined, "user_already_joined"}, St};
        false -> 
            A = genserver:request(St#client_st.server, {join, list_to_atom(Channel), St#client_st.nick, self(), ""}), 
            case A of 
                ok ->
                    % io:fwrite("the first client channel join ~n", []),
                    % io:fwrite("nick: ~p~n ", [St#client_st.nick]),
                    NewList = lists:append(St#client_st.channel_list, [Channel]),
                    NewClientSt = St#client_st{channel_list = NewList},
                    {reply, ok, NewClientSt};
                alreadyInChannel ->
                    % If we ever get here, then there is an inconsistency between the client and channels.
                    % The client thinks it isn't a member of the requested channel at the same time as the channel
                    % has the client logged as a member.
                    NewList = lists:append(St#client_st.channel_list, [Channel]),
                    NewClientSt = St#client_st{channel_list = NewList},
                    {reply, {error, user_already_joined, "user_already_joined"}, NewClientSt};
                _ ->
                    {reply, {error, A,  "Error in join"}, St}
            end
    end;    

% Leave channel
handle(St, {leave, Channel}) ->
    case lists:member(Channel ,St#client_st.channel_list) of
        true -> 
            A = genserver:request(St#client_st.server, {leave, list_to_atom(Channel), St#client_st.nick, self(), ""}),
            case A of
                ok -> 
                    NewList = lists:delete(Channel, St#client_st.channel_list),
                    NewClientSt = St#client_st{channel_list = NewList},
                    {reply, A, NewClientSt};
                channel_doesnt_have_client ->
                    not_implemented;
                _ ->
                    {reply, {error, Channel, "error occurred"}, St}
            end;

        false -> 
            {reply, {error, Channel, "user_not_joined"}, St}
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    case lists:member(Channel ,St#client_st.channel_list) of
        true ->
            Reply = catch genserver:request(St#client_st.server, {message_send, list_to_atom(Channel), St#client_st.nick, self(), Msg}),
            case Reply of  
                ok ->
                    {reply, Reply, St};
                _ ->
                    {reply, {error, Channel, "user_not_joined"}, St}
            end;
        false ->
            {reply, {error, Channel, "user_not_joined"}, St}
    end;

%----------------------------------------------------------------------------------------------------------
% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    io:fwrite("the data 3: ~n~p", [Data]),
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
