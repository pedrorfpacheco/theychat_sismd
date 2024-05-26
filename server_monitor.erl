-module(server_monitor).
-export([startMonitor/4, add_machine/1, reboot_serverMonitor/4, monitorServer/4]).

startMonitor(ServerPid, ServerName, Router, Machine) ->
  MonitorPid = spawn(fun() -> monitorServer(ServerPid, ServerName, Router, Machine) end),
  io:format("Server Monitor pid at ~p~n", [MonitorPid]),
  MonitorPid.

add_machine(Machine) ->
  net_adm:ping(Machine).

monitorServer(ServerPid, ServerName, Router, Machine) ->
  erlang:monitor(process, ServerPid),
  {Router, Machine} ! {join, self(), ServerName, ServerPid},
  receive
    {'DOWN', _Ref, process, ServerPid, Reason} ->
      io:format("Server ~p exited with reason: ~p. Rebooting...~n", [ServerPid, Reason]),
      NewServerPid = server:reboot(ServerName),
      monitorServer(NewServerPid, ServerName, Router, Machine)
  end.

reboot_serverMonitor(ServerName, ServerPid, RouterPid, Machine) ->
  NewServerMonitorPid = spawn(fun() -> startMonitor(ServerName, ServerPid, RouterPid, Machine) end),
  io:format("Server Monitor pid at ~p~n", [NewServerMonitorPid]),
  NewServerMonitorPid.