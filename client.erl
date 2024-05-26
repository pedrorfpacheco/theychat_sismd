-module(client).
-export([start/3, add_machine/1, loop/3, send_msg/2]).

start(Name, RouterName, Machine) ->
  UserPid = spawn(fun() ->
    loop(undefined, undefined, Name)
                  end),
  register(Name, UserPid),
  {RouterName, Machine} ! {connect, UserPid, Name}.

add_machine(Machine) ->
  net_adm:ping(Machine).

send_msg(Username, Message) ->
  {Username, node(self())} ! {Message}.

loop(ServerPid, ServerName, Username) ->
  receive
    {joined, ServerPid1, ServerName1} ->
      io:format("Joined the chat server!~n"),
      loop(ServerPid1, ServerName1, Username);
    {message, User, Message} ->
      io:format("~p: ~p~n", [User, Message]),
      loop(ServerPid, ServerName, Username);
    {Message} ->
      case Message of
        "exit" ->
          io:format("Exiting...~n"),
          {ServerName, ServerPid} ! {leave, self(), Username, Message},
          receive
            {_, left} ->
              io:format("Left the chat server!~n"),
              exit(normal)
          end;
        _ ->
          {ServerName, node(ServerPid)} ! {send, self(), Username, Message},
          loop(ServerPid, ServerName, Username)
      end;
    {'DOWN', _, _, _, _} ->
      io:format("Server ~p is down!~n", [ServerName]),
      exit(normal);
    Other ->
      io:format("Received unsupported message: ~p~n", [Other]),
      loop(ServerPid, ServerName, Username)
  end.