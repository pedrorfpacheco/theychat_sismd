-module(client).
-export([enterServer/0,add_remote/1,start/0, send_msg/6, loop/3, chat/1]).

start() ->
    spawn(fun() ->
        timer:sleep(3000),  % Delay for 5000 milliseconds
        enterServer()
          end).

add_remote(RemoteMachine) ->
    net_adm:ping(RemoteMachine).

send_msg(Action,Nickname,ClientPid,Server,RemoteMachine,Message)->
    {Server,RemoteMachine} ! {Action,ClientPid,Nickname,Message}.

enterServer() ->
    io:format("Enter the server name:~n"),
    Server = string:strip(string:strip(io:get_line(""), both, $\n), both, $"),
    io:format("Enter the remote machine name:~n"),
    RemoteMachine = string:strip(string:strip(io:get_line(""), both, $\n), both, $"),
    io:format("Connecting to ~s on ~s~n", [Server, RemoteMachine]),
    RemoteMachineAtom = list_to_atom(RemoteMachine),
    ServerAtom = list_to_atom(Server),
    io:format("Enter your nickname:~n"),
    Nickname = string:strip(io:get_line(""), both, $\n),
    RegisterClientName = list_to_atom(Nickname), 
    PidC= self(),  
    PidChat = spawn(fun() -> chat(PidC) end),
    register(chatInput, PidChat),  
    register(RegisterClientName, PidC),
    io:format("Enter password (leave blank if server is public):~n"),
    Password = string:strip(string:strip(io:get_line(""), both, $\n), both, $"),
    PasswordAtom = list_to_atom(Password),
    send_msg(join,Nickname,PidC,ServerAtom,RemoteMachineAtom),
    monitor(process, {ServerAtom,RemoteMachineAtom}),
    io:format("Enter a message:~n"),
    loop(ServerAtom, RemoteMachineAtom, Nickname).

chat(Pid) ->
    Message = string:strip(io:get_line(""), both, $\n),
    Pid ! {Message},
    case Message of
        "quit" ->
            exit(normal);
        _ ->
            chat(Pid)
    end.
    
loop(Server, RemoteMachineAtom, Nickname) ->
    receive
        {message, Pseudo, Message} ->
            io:format("~p: ~p~n", [Pseudo, Message]),
            loop(Server, RemoteMachineAtom, Nickname);
        {Message} ->
            case Message of 
                "quit" ->
                    io:format("Quitting...~n"),
                    send_msg(leave,Nickname,self(),Server,RemoteMachineAtom,""),
                    receive
                        {_,left} ->
                            io:format("Left the chat server!~n"),
                            exit(normal)
                    end;
                _ ->
                    send_msg(send,Nickname,self(),Server,RemoteMachineAtom,Message),
                    loop(Server, RemoteMachineAtom, Nickname)
            end;
        {_,joined} ->
            io:format("Joined the chat server!~n"),
            loop(Server, RemoteMachineAtom, Nickname);
        {'DOWN', _, _, _, _} ->
            io:format("Server ~p is down!~n", [Server]),
            exit(normal);
        {broadJoin, Pseudo} ->
            io:format("~p joined the chat server!~n", [Pseudo]),
            loop(Server, RemoteMachineAtom, Nickname);
        {broadLeft, Pseudo} ->
            io:format("~p left the chat server!~n", [Pseudo]),
            loop(Server, RemoteMachineAtom, Nickname);
        {_,wrongPassword} ->
            io:format("Wrong password!~n"),
            exit(normal);
        Other ->
            io:format("Received unsupported message: ~p~n", [Other]),
            loop(Server, RemoteMachineAtom, Nickname)
        end.