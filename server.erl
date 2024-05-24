-module(server).
-export([start/1]).

start(Server) ->
    register(Server, spawn(fun() -> init([], Server) end)).

init(Users, Server) ->
    handleClients(Users , Server).

handleClients(Users, Server) ->
    receive
        {join, UserPid, Username, _} ->
            monitor(process, UserPid),
            io:format("User with username ~p joined the server ~p! :)~n", [Username, Server]),
            NewUsers = [UserPid | Users],
            UserPid ! {self(), joined},
            broadcast_join(UserPid, Username, NewUsers),
            io:format("New users: ~p~n", [NewUsers]),
            handleClients(NewUsers, Server);
        {leave, UserPid, Username, _} ->
            io:format("User with username ~p left the server! :(~n", [Username]),
            NewUsers = lists:delete(UserPid, Users),
            io:format("New users: ~p~n", [NewUsers]),
            UserPid ! {self(), left},
            broadcast_left(UserPid, Username, NewUsers),
            handleClients(NewUsers, Server);
        {send, UserPid, Username, Message} ->
            io:format("Message from ~p: ~p~n", [Username, Message]),
            broadcast_message(UserPid, Username, Message, Users),
            handleClients(Users, Server);
        {'DOWN', _, _, UserPid, _} ->
            io:format("User ~p died!~n", [UserPid]),
            NewUsers = lists:delete(UserPid, Users),
            broadcast_left(UserPid, UserPid, NewUsers),
            io:format("New users: ~p~n", [NewUsers]),
            handleClients(NewUsers, Server);
        Other ->
            io:format("Received unsupported message: ~p~n", [Other]),
            handleClients(Users, Server)
    end.

broadcast_message(_, _, _, []) ->
    ok;
broadcast_message(UserPid, Username, Message, [User | RestClients]) when User == UserPid ->
    broadcast_message(UserPid, Username, Message, RestClients);
broadcast_message(UserPid, Username, Message, [User | RestClients]) ->
    User ! {message, Username, Message},
    broadcast_message(UserPid, Username, Message, RestClients).

broadcast_join(_, _, []) ->
    ok;
broadcast_join(UserPid, Username, [User | RestClients]) when User == UserPid ->
    broadcast_join(UserPid, Username, RestClients);
broadcast_join(UserPid, Username, [User | RestClients]) ->
    User ! {broadJoin, Username},
    broadcast_join(UserPid, Username, RestClients).

broadcast_left(_, _, []) ->
    ok;
broadcast_left(UserPid, Username, [User | RestClients]) when User == UserPid ->
    broadcast_left(UserPid, Username, RestClients);
broadcast_left(UserPid, Username, [User | RestClients]) ->
    User ! {broadLeft, Username},
    broadcast_left(UserPid, Username, RestClients).