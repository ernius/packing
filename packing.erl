%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2024, 
%%% @doc  parallel_packing/2 implements the packing algorithm, the max amount of spawn threads can be configured with MAX_SPAWN macro
%%% proof_of_custody/2 and validates_proof_of_custody/4 generates and validates a proof of custody
%%% test/0 function runs an example usage of previous functions following the specified exercise steps.
%%%
%%% @author Ernesto
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------
-module(packing).
-export([ test/0
        , test_sha256/0
        , packing/3
        , parallel_packing/2
        , sequential_packing/2
        , network_data/0
        , miner_address/0
        , block_hash/0
        , proof_of_custody/2
        , validate_proof_of_custody/4
        , parallel_hash/4
        ]).

-import(hash, [sequential_hash/2]).

-define(BUFFER_BYTES_SIZE, 65536 ). % 64*1024
-define(OFFSET_RANGE, 2048). % BUFFER_BYTES_SIZE / CHUNK_BYTES_SIZE
-define(CHUNK_BYTES_SIZE, 32).
-define(ADDRESS_BYTES_SIZE, 32).
-define(PROOF_OF_CUSTODY_LENGTH, 10).

%% parallelism configurable parameter: maximum number of spawn threads.
-define(MAX_SPAWN, 500). % 1 < MAX_SPAWN <= OFFSET_RANGE

%% @doc test solution
test() ->
    io:format("generating random data and miner addresses ... \n", []),
    %% 1. Fill 64KB network data ------------------------------------------------------------
    Data = network_data(),

    %% io:format("data:        ~p\n",[binary:encode_hex(Data)]),
    %% 2. Fill 32B miner address
    MinerAddress = miner_address(),
    io:format("miner address: ~p\n",[binary:encode_hex(MinerAddress)]),

    %% 3. & 4. Packing ------------------------------------------------------------

    %% first sequential fist iteration 
    %% io:format("starting packing sequentially ...\n", []),
    %% {TimeSec, PackedBufferSeq} = timer:tc(?MODULE, sequential_packing, [MinerAddress, Data]),
    %% io:format("sequential time: ~p seconds\n", [TimeSec/1000000]),

    io:format("starting packing in parallel ...\n", []),
    {TimeParallel, PackedBufferParallel} = timer:tc(?MODULE, parallel_packing, [MinerAddress, Data]),
    io:format("finish packing in parallel in: ~p seconds\n", [TimeParallel/1000000]),
    %% io:format("packed: ~p\n",[binary:encode_hex(PackedBufferParallel)]),

    %% compare sequential and parallel results
    %% if
    %%     PackedBufferSeq =:= PackedBufferParallel -> io:format("equal");
    %%     true -> io:format("not equal")
    %% end,

    %% 6. Fill a 32B recent block hash  ------------------------------------------------------------
    io:format("generating random block hash ...\n", []),
    BlockHash = block_hash(),
    io:format("block hash: ~p\n",[binary:encode_hex(BlockHash)]),

    %% 7. ^ 8. Generate the proof of custody ------------------------------------------------------------
    io:format("generating proof of custody ...\n", []),
    Proof = proof_of_custody(PackedBufferParallel, BlockHash),
    io:format("proof of custody: ~p\n",[[ProofHex || ProofHex <- lists:map(fun binary:encode_hex/1, Proof)]]),

    %% 9. Validate the proof of custody ------------------------------------------------------------
    io:format("validating proof of custody ...\n", []),
    ValidationResult = validate_proof_of_custody(MinerAddress, BlockHash, Data, Proof),
    if
	ok =:= ValidationResult ->
	    io:format("Validation ended OK\n", []);
	true ->
	    io:format("Error in validation!\n", [])
    end.

%% simple test to verify hash nif mechanism against crypto library
test_sha256() ->
    B = block_hash(),
    io:format("data:        ~p\n",[binary:encode_hex(B)]),
    Hash = hash:hash(B),
    io:format("hash         ~p\n",[binary:encode_hex(Hash)]),
                                                % check against crypto library
    HashCrypto = crypto:hash(sha256,B),
    io:format("hash crypto: ~p\n",[binary:encode_hex(HashCrypto)]),
    if
	Hash =:= HashCrypto ->
	    io:format("Hashes match OK\n", []);
	true ->
	    io:format("Error in hashing!\n", [])
    end.

%% random entropy functions ==========================================================================================

-spec network_data() -> binary().
%% @doc generates 64KB random binary
network_data() ->
    crypto:strong_rand_bytes(?BUFFER_BYTES_SIZE).

-spec miner_address() -> binary().
%% @doc generates 32B random binary representing a miner address
miner_address() ->
    crypto:strong_rand_bytes(?ADDRESS_BYTES_SIZE).

-spec block_hash() -> binary().
%% @doc generates 32B random binary representing a block hash
block_hash() ->
    crypto:strong_rand_bytes(?CHUNK_BYTES_SIZE).

-spec offset(Seed :: binary()) -> pos_integer().
%% @doc returns a pseudo random (deterministic) offset in Bytes
%% @param Seed entropy seed 
offset(Seed) ->
    rand:seed(exsss,lists:sum(binary_to_list(Seed))),
    rand:uniform(?OFFSET_RANGE)-1.

%% Packing  ===========================================================================================================
-spec packing(Data :: binary(), MinerAddress :: binary(), BlockHash :: binary()) -> {binary(),binary()}.
%% @doc Packs Data and computes its proof of custody
%% @param Data 64KB binary
%% @param MinerAddress 32B binary
%% @param BlockHash 32B binary
%% @returns tuple of packed data and its proof of custody
packing(Data, MinerAddress, BlockHash) ->
    PackedData = parallel_packing(MinerAddress, Data),
    {PackedData, proof_of_custody(PackedData, BlockHash)}.

-spec parallel_packing(MinerAddress :: binary(), Buffer :: binary()) -> binary().
%% @doc Sequentially packs Buffer with miner address 
%% @param MinerAddres 32B binary
%% @param Buffer 64KB binary to be packed
parallel_packing(MinerAddress, Buffer) ->
    Self = self(),
    [spawn_link(?MODULE,parallel_hash, [Self, MinerAddress, Offset, binary:part(Buffer, Offset*?CHUNK_BYTES_SIZE, ?CHUNK_BYTES_SIZE)]) || Offset <- lists:seq(0, ?MAX_SPAWN - 1) ],
    gather(Self, MinerAddress, Buffer, 0, ?MAX_SPAWN, <<>>).

parallel_hash(Self, MinerAddress, Offset, Chunk) ->
    %% io:format("evaluating offset:~p\n", [Offset]),
    Hash = hash:sequential_hash(MinerAddress, Chunk),
    Self ! {Offset, Hash}.

gather(Self, MinerAddress, Buffer, Offset, NextOffsetToSpawn, Acc) when Offset < ?OFFSET_RANGE ->
    receive
	{Offset, Hash} -> 
            %% io:format("received offset:~p\n", [Offset]),
	    if
		NextOffsetToSpawn < ?OFFSET_RANGE ->
		    spawn_link(?MODULE,parallel_hash, [Self, MinerAddress, NextOffsetToSpawn, binary:part(Buffer, NextOffsetToSpawn*?CHUNK_BYTES_SIZE, ?CHUNK_BYTES_SIZE)]);
		true -> noop
	    end,
	    gather(Self, MinerAddress, Buffer, Offset+1, NextOffsetToSpawn+1, <<Acc/binary,Hash/binary>>)
    end;
gather(_,_,_,_,_,Acc) -> Acc.

-spec sequential_packing(MinerAddress :: binary(), Buffer :: binary()) -> binary().
%% @doc Sequentially packs Buffer with miner address wrote as a simple first version and to compare performance
%% @param MinerAddres 32B binary
%% @param Buffer 64KB binary to be packed
sequential_packing(MinerAddress, Buffer) ->
    sequential_acc(MinerAddress, Buffer, 0, <<>>).

%% tail recursive function accumulating packed result in Acc
sequential_acc(MinerAddress, Buffer, Pos, Acc) when Pos < ?BUFFER_BYTES_SIZE ->
    <<_:Pos/binary,Chunk:?CHUNK_BYTES_SIZE/binary,_/binary>> = Buffer,
    %% erlang:display(Chunk),
    Hash = hash:sequential_hash(MinerAddress, Chunk),
    %% erlang:display(Hash),
    sequential_acc(MinerAddress, Buffer, Pos + ?CHUNK_BYTES_SIZE, <<Acc/binary,Hash/binary>>);
sequential_acc(_,_,_,Acc) -> Acc.


%% Proof of Custody  =======================================================================================================
-spec proof_of_custody(Buffer :: binary(), BlockHash :: binary()) -> [binary()].
%% @doc Generates a ten (?PROOF_OF_CUSTODY_LENGTH) length hashes list representing the proof of custody
%% @param Packed buffer 64KB binary to be packed
%% @param BlockHash 32B binary
%% @returns a ten length hashes list proof of custody 
proof_of_custody(Buffer, BlockHash) ->    
    proof_of_custody_acc(Buffer, BlockHash, [], ?PROOF_OF_CUSTODY_LENGTH).

proof_of_custody_acc(Buffer, BlockHash, Proof, N) when N > 0  ->    
    Offset = offset(BlockHash)*?CHUNK_BYTES_SIZE,
    <<_:Offset/binary,Chunk:?CHUNK_BYTES_SIZE/binary,_/binary>> = Buffer,
    Hash = hash:sequential_hash(BlockHash, Chunk),
    proof_of_custody_acc(Buffer, Hash, [Hash|Proof], N - 1);
proof_of_custody_acc(_Buffer, _BlockHash, Proof, 0) -> lists:reverse(Proof).

-spec validate_proof_of_custody(MinerAddres :: binary(), BlockHash :: binary(), Buffer :: binary(), ProofOfCustody :: [binary()]) -> {ok|error}.
%% @doc Validates a Proof of Cusody given the unpacked buffer
%% @param Packedk buffer 64KB binary to be packed
%% @param BlockHash 32B binary
%% @returns ok if validation succeed, error otherwise
validate_proof_of_custody(MinerAddress, BlockHash, Buffer, [Hash|Proof]) -> 
    Offset = offset(BlockHash)*?CHUNK_BYTES_SIZE,
    <<_:Offset/binary,Chunk:?CHUNK_BYTES_SIZE/binary,_/binary>> = Buffer,
    HashMiner = hash:sequential_hash(MinerAddress, Chunk),
    HashProof = hash:sequential_hash(BlockHash, HashMiner),
    if 
	Hash =:= HashProof ->
	    validate_proof_of_custody(MinerAddress, HashProof, Buffer, Proof);
	true -> 
	    error
    end;
validate_proof_of_custody(_MinerAddress, _BlockHash, _Buffer, []) -> ok.


