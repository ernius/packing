-module(hash).

-export([sequential_hash/2, hash/1]).
-nifs([sequential_hash/2, hash/1]).

-on_load(init/0).

init() ->
    ok = erlang:load_nif("./hash_nif", 0).

-spec sequential_hash(MiningAddresses :: binary(), Buffer :: binary()) -> binary().
%% @doc generates the hash of the two parameters and rehashes it 1000000
%% @param MiningAddresses 32B binary
%% @param Buffer 32B binary
sequential_hash(_,_) ->
    erlang:nif_error(nif_library_not_loaded).

-spec hash(Buffer :: binary()) -> binary().
%% @doc generates the SHA256 hash 
%% @param Buffer 32B binary
hash(_) ->
    erlang:nif_error(nif_library_not_loaded).
