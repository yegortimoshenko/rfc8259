%% Program to make Macs sing together at the same time.
%% Best at office at night.

-module(choir).
-export([start/0, start_world/0, say/1, say/3]).

start() -> register(?MODULE, spawn(fun loop/0)).

loop() ->
    receive {Message, Voice} ->
        os:cmd(io_lib:format("say -v '~s' '~s'", [Voice, Message])),
        loop()
    end.

world() -> [node()|nodes()].

start_world() -> rpc:multicall(world(), ?MODULE, start, []).

say(Message) -> say(Message, "Kyoko").

say(Message, Voice) -> say(Message, Voice, world()).

say(Message, Voice, [H|T]) -> 
    {?MODULE, H} ! {Message, Voice},
    say(Message, Voice, T);
say(_, _, []) -> ok.
