-module(router).
-export([start/2, reboot_router/1]).

start(RouterName, Machine) ->
  RouterPid = spawn(fun() -> init([], [], Machine) end),
  io:format("Router PID ~p~n", [RouterPid]),
  register(RouterName, RouterPid),
  router_monitor:start(RouterPid, Machine),
  RouterPid.

init(ServerMonitors, Users, Machine) ->
  manage_monitors(ServerMonitors, Users, Machine).

manage_monitors(ServerMonitors, Users, Machine) ->
  io:format("ServerMonitors: ~p, Users: ~p, Machine: ~p~n",
    [ServerMonitors, Users, Machine]),
  receive
    {join, ServerMonitorPid, ServerName, ServerPid} ->
      erlang:monitor(process, ServerMonitorPid),
      NewServerMonitor = {ServerMonitorPid, ServerName, ServerPid},
      NewServerMonitors = [NewServerMonitor | ServerMonitors],
      io:format("Router added Server Monitor ~p~n", [ServerMonitorPid]),
      manage_monitors(NewServerMonitors, Users, Machine);
    {'DOWN', _Ref, process, ServerMonitorPid, Reason} ->
      ServerMonitor = lists:keyfind(ServerMonitorPid, 1, ServerMonitors),
      io:format("ServerMonitor ~p exited with reason: ~p. ~n~nRebooting...~n~n", [ServerMonitorPid, Reason]),
      {ServerMonitorPid, ServerName, ServerPid} = ServerMonitor,
      NewServerMonitorPid = server_monitor:reboot_serverMonitor(ServerName, ServerPid, self(), Machine),
      NewServerMonitor = {NewServerMonitorPid, ServerName, ServerPid},
      NewServerMonitors = [NewServerMonitor | ServerMonitors],
      ServerMonitors = lists:delete(ServerMonitor, ServerMonitors),
      manage_monitors(NewServerMonitors, Users, Machine);
    {connect, UserPid, Username} ->
      manage_users(ServerMonitors, Users, UserPid, Username, Machine);
    Other ->
      io:format("Unhandled message: ~p~n", [Other]),
      manage_monitors(ServerMonitors, Users, Machine)
  end.


manage_users(ServerMonitors, Users, UserPid, Username, Machine) ->
  case ServerMonitors of
    [] ->
      io:format("No servers available to handle user ~p~n", [UserPid]),
      manage_monitors(ServerMonitors, Users, Machine);
    _ ->
      ServerCount = length(ServerMonitors),
      CurrentIndex = length(Users) rem ServerCount,
      {_, ServerName, ServerPid} = lists:nth(CurrentIndex + 1, ServerMonitors),
      io:format("~p assigned to server ~p~n", [UserPid, ServerPid]),
      {ServerName, node(ServerPid)} ! {join, UserPid, Username},
      NewUsers = [UserPid | Users],
      manage_monitors(ServerMonitors, NewUsers, Machine)
  end.

reboot_router(Machine) ->
  Pid = spawn(fun() -> init([], [], Machine) end),
  io:format("Router pid at ~p~n", [Pid]),
  Pid.
