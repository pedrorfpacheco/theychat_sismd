-module(router_monitor).
-export([start/2]).

start(RouterPid, Machine) ->
  RouterMonitorPid = spawn(fun() -> monitorRouter(RouterPid, Machine) end),
  io:format("RouterMonitor pid ~p~n", [RouterMonitorPid]),
  RouterMonitorPid.

monitorRouter(RouterPid, Machine) ->
  erlang:monitor(process, RouterPid),
  receive
    {'DOWN', _Ref, process, RouterPid, Reason} ->
      io:format("Router ~p exited with reason: ~p. ~n~nRebooting...~n~n", [RouterPid, Reason]),
      NewRouterPid = router:reboot_router(Machine),
      monitorRouter(NewRouterPid, Machine)
  end.