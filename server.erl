-module(server).
-export([start/3, add_machine/1, reboot/1]).

start(ServerName, RouterPid, Machine) ->
  io:format("Server started at ~p~n", [ServerName]),
  Pid = spawn(fun() -> init([], ServerName) end),
  register(ServerName, Pid),
  io:format("Server pid at ~p~n", [Pid]),
  server_monitor:startMonitor(Pid, ServerName, RouterPid, Machine),
  Pid.

init(Users, ServerName) ->
  handle_users(Users, ServerName).

add_machine(Machine) ->
  net_adm:ping(Machine).

handle_users(Users, ServerName) ->
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
      handle_users(NewUsers, ServerName);
    {leave, UserPid, Username, _} ->
      io:format("~p as left the server ~p!~n", [Username, ServerName]),
      NewUsers = lists:delete(UserPid, Users),
      io:format("New users: ~p~n", [NewUsers]),
      UserPid ! {self(), left},
      lists:foreach(fun({Name, Pid}) -> {Name, node(Pid)} ! {message, Username, "left"} end, NewUsers),
      handle_users(NewUsers, ServerName);
    {send, _, Username, Message} ->
      io:format("Message from ~p: ~p~n", [Username, Message]),
      lists:foreach(fun({Name, Pid}) -> {Name, node(Pid)} ! {message, Username, Message} end, Users),
      handle_users(Users, ServerName);
    {'DOWN', _, _, UserPid, _} ->
      io:format("~p died!~n", [UserPid]),
      NewUser = lists:keyfind(UserPid, 2, Users),
      NewUsers = lists:delete(NewUser, Users),
      lists:foreach(fun({Name, Pid}) -> {Name, node(Pid)} ! {message, UserPid, "died"} end, NewUsers),
      io:format("New users: ~p~n", [NewUsers]),
      handle_users(NewUsers, ServerName);
    Other ->
      io:format("Unsupported message: ~p~n", [Other]),
      handle_users(Users, ServerName)
  end.

reboot(ServerName) ->
  io:format("Server started ~p~n", [ServerName]),
  Pid = spawn(fun() -> init([], ServerName) end),
  io:format("Server pid at ~p~n", [Pid]),
  Pid.