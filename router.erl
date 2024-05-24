-module(router).
-export([start/0]).

start() ->
  spawn(fun() -> init() end).

init() ->
  Pid1 = spawn(fun)