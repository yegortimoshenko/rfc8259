-module(http_query).
-export([encode/1]).

encode([{K,V}]) -> encode(K, V);
encode([{K,V}|T]) -> [encode(K,V) | [$& | encode(T)]];
encode(M) when is_map(M) -> encode(maps:to_list(M)).

encode(_, []) -> [];

encode(K, [{X,Y}|T]) ->
    MarkedKey = [K, $[, X, $]],
    [encode(MarkedKey, Y)|encode(K, T)];

encode(K, L) when is_list(L) ->
    MarkedKey = [K, "[]"],
    encode(lists:map(fun(X) -> {MarkedKey, X} end, L));

encode(K, M) when is_map(M) -> encode(K, maps:to_list(M));
encode(K, V) -> io_lib:format("~s=~s", [K, encode_value(V)]).

encode_value(B) when is_binary(B) -> http_uri:encode(binary_to_list(B));
encode_value(I) when is_integer(I) -> integer_to_list(I);
encode_value(F) when is_float(F) -> float_to_list(F).
