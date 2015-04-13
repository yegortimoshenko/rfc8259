%% Program to make Macs sing together at the same time.
%% Best at office at night.

-module(choir).
-export([start/0, start_world/0, say/1, say/2]).

start() -> register(?MODULE, spawn(fun loop/0)).

loop() ->
    receive Msg ->
        os:cmd("say '" ++ Msg ++ "'"),
        loop()
    end.

world() -> [node()|nodes()].

start_world() -> rpc:multicall(world(), ?MODULE, start, []).

say(Message) -> say(Message, world()).

say(Message, [H|T]) -> 
    {?MODULE, H} ! {self(), Message},
    say(Message, T);
say(_, []) -> ok.
