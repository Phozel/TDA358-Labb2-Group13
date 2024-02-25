-module(server).
-export([start/1,stop/1,test_for_print/1,ping/0]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, [],fun InsertMetod som "handles it" här). 
    % sen kan vi pattern matcha för att göra olika saker beroende på vad vi får
    
    %spawn(server, test_for_print, [10]).
    % Pid1 = spawn(genserver, start, [ServerAtom]), %ksk använda sig av en record så som client.erl gör?
    % io:fwrite("next is spawn ~n", []),
    % Pid2 = spawn(?MODULE, ping, []),
    % io:fwrite("after spawn ~n", []),
    % Pid2 ! "tjosan!",
    % Pid2 ! "hejhopp".


ping() -> 
    io:fwrite("got to ping ~n", []),
    receive
        From -> io:fwrite("Ping! ~p~n", [From]),
        ping();
        _ -> ignore
    after 10000 -> timeout
    end.

% Test method to learn how to print things :)
test_for_print(X) ->
    if  X >= 0 -> 
            io:fwrite("more than 0 : ~p~n", [X]),
            test_for_print(X-1);
        X > -10 ->
            io:fwrite("~p~n", [X]),
            test_for_print(X-1);
        true -> 
            io:format("end of if~n")
    end.
    


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
