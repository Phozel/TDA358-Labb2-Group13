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
        %server_st = server:initial_server_state(ServerAtom, [])
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
    % TODO: Implement this function
    % {reply, ok, St} ;
    io:fwrite("the first client channel join ~n", []),
    io:fwrite("nick: ~p~n ", [St#client_st.nick]),
    %A = catch genserver:request(St, {join, Channel, ServerSt}),
    %A = genserver:request(list_to_atom(St#client_st.nick), {join, Channel}),    %:server_loop(server, St, {join, Channel}),
    {A, ChannelAtom} = genserver:request(St#client_st.server, {join, list_to_atom(Channel), St#client_st.nick, ""}), 
    io:fwrite("we came back: ~n", []),
    io:fwrite("A: ~n~p", [A]),
    %io:fwrite("B: ~n~p", [B]),
    case A of 
        
        ok ->
            NewList = lists:append(St#client_st.channel_list, ChannelAtom),
            NewClientSt = St#client_st{channel_list = NewList},
            %{A, B};
            %lists:
            %add channel to channel list
            io:fwrite("ok in client: ~n", []),
            {reply, ok, NewClientSt};
        % alreadyInChannel ->
        %     {reply, }
        _ ->
            {reply, {error, A,  "Error in join"}, St}
    end;
    %{reply, Data, NewState}

% Leave channel
handle(St, {leave, Channel}) ->
    {A, ChannelAtom} = genserver:request(St#client_st.server, {leave, list_to_atom(Channel), St#client_st.nick, ""}),

    case A of

        ok -> 
            NewList = lists:delete(ChannelAtom, St#client_st.channel_list),
            NewClientSt = St#client_st{channel_list = NewList},

            {reply, A, NewClientSt};
        _ ->
            {reply, {error, Channel, "user_not_joined"}, St}
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->

    {A, ChannelAtom} = genserver:request(St#client_st.server, {message_send, list_to_atom(Channel), St#client_st.nick, Msg}),
    % TODO: Implement this function
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "message sending not implemented"}, St} ;

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
