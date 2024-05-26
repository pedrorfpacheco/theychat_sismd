-module(server).
-export([start/3, reboot_server/1]).

start(ServerName, RouterPid, Machine) ->
  io:format("Server started at ~p~n", [ServerName]),
  Pid = spawn(fun() -> init([], ServerName) end),
  register(ServerName, Pid),
  io:format("Server pid at ~p~n", [Pid]),
  server_monitor:start(Pid, ServerName, RouterPid, Machine),
  Pid.

init(Users, ServerName) ->
  manage_users(Users, ServerName).

manage_users(Users, ServerName) ->
  receive
    {join, UserPid, Username} ->
      monitor(process, UserPid),
      io:format("~p joined the server ~p!~n", [Username, ServerName]),
      NewUsers = [{Username, UserPid} | Users],
      io:format("~p~n", [NewUsers]),
      ServerPid = whereis(ServerName),
      {Username, node(UserPid)} ! {joined, ServerPid, ServerName},
      lists:foreach(fun({Name, Pid}) -> {Name, node(Pid)} ! {message, Username, "joined"} end, NewUsers),
      io:format("New users: ~p~n", [NewUsers]),
      manage_users(NewUsers, ServerName);
    {leave, UserPid, Username, _} ->
      io:format("~p as left the server ~p!~n", [Username, ServerName]),
      NewUsers = lists:delete(UserPid, Users),
      io:format("New users: ~p~n", [NewUsers]),
      UserPid ! {self(), left},
      lists:foreach(fun({Name, Pid}) -> {Name, node(Pid)} ! {message, Username, "left"} end, NewUsers),
      manage_users(NewUsers, ServerName);
    {send, _, Username, Message} ->
      io:format("Message from ~p: ~p~n", [Username, Message]),
      lists:foreach(fun({Name, Pid}) -> {Name, node(Pid)} ! {message, Username, Message} end, Users),
      manage_users(Users, ServerName);
    {'DOWN', _, _, UserPid, _} ->
      io:format("~p died!~n", [UserPid]),
      NewUser = lists:keyfind(UserPid, 2, Users),
      NewUsers = lists:delete(NewUser, Users),
      lists:foreach(fun({Name, Pid}) -> {Name, node(Pid)} ! {message, Name, "died"} end, NewUsers),
      io:format("New users: ~p~n", [NewUsers]),
      manage_users(NewUsers, ServerName);
    Other ->
      io:format("Unsupported message: ~p~n", [Other]),
      manage_users(Users, ServerName)
  end.

reboot_server(ServerName) ->
  io:format("Server started ~p~n", [ServerName]),
  Pid = spawn(fun() -> init([], ServerName) end),
  io:format("Server pid at ~p~n", [Pid]),
  Pid.