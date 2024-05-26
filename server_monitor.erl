-module(server_monitor).
-export([start/4, reboot_serverMonitor/4]).

start(ServerPid, ServerName, Router, Machine) ->
  MonitorPid = spawn(fun() -> monitorServer(ServerPid, ServerName, Router, Machine) end),
  io:format("Server Monitor pid at ~p~n", [MonitorPid]),
  MonitorPid.

monitorServer(ServerPid, ServerName, Router, Machine) ->
  erlang:monitor(process, ServerPid),
  {Router, Machine} ! {join, self(), ServerName, ServerPid},
  receive
    {'DOWN', _Ref, process, ServerPid, Reason} ->
      io:format("Server ~p exited with reason: ~p. ~n~nRebooting...~n~n", [ServerPid, Reason]),
      NewServerPid = server:reboot_server(ServerName),
      monitorServer(NewServerPid, ServerName, Router, Machine)
  end.

reboot_serverMonitor(ServerName, ServerPid, RouterPid, Machine) ->
  NewServerMonitorPid = spawn(fun() -> start(ServerName, ServerPid, RouterPid, Machine) end),
  io:format("Server Monitor pid at ~p~n", [NewServerMonitorPid]),
  NewServerMonitorPid.