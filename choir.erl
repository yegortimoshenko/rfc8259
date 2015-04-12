-module(choir).
-export([start/0, start_world/0, say/1, say/2]).

%%% Program to choir some words on multiple computers at the same time.
%%% Best at office. OS X only. 

start() -> register(?MODULE, spawn(fun loop/0)).

loop() ->
    receive Msg ->
        os:cmd("say '" ++ Msg ++ "'"),
        loop()
    end.

world() -> [node()|nodes()].

start_world() -> rpc:multicall(world(), ?MODULE, start, []).

say(Message) -> say(world(), Message).

say([H|T], Message) -> 
    {?MODULE, H} ! {self(), Message},
    say(T, Message);
say([], _) -> ok.
