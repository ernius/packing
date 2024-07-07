#include <string.h>
#include "usr/include/erl_nif.h"

#define CHUNK_SIZE 32  /* in Bytes */

extern void hash_data_chunk(unsigned char * mining_address, unsigned char * data_chunck, unsigned char * data_chunck_hash);
extern void hash(unsigned char * data, unsigned char * data_hash);

static ERL_NIF_TERM sequential_hash(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

    ErlNifBinary mining_address;
    ErlNifBinary data_chunk;
    ERL_NIF_TERM data_chunk_hash;
    unsigned char * data_chunk_hash_buffer = enif_make_new_binary(env, CHUNK_SIZE, &data_chunk_hash);

    if (!enif_inspect_binary(env, argv[0], &mining_address)) {
        return enif_make_atom(env, "error_mining_address");
    }
    if (!enif_inspect_binary(env, argv[1], &data_chunk)) {
        return enif_make_atom(env, "error_data_chunk");
    }

    hash_data_chunk(mining_address.data, data_chunk.data, data_chunk_hash_buffer);
    
    return data_chunk_hash;
}

static ERL_NIF_TERM hash256(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

    ErlNifBinary data;
    ERL_NIF_TERM data_hash;
    unsigned char * data_hash_buffer = enif_make_new_binary(env, CHUNK_SIZE, &data_hash);

    if (!enif_inspect_binary(env, argv[0], &data)) {
        return enif_make_atom(env, "error_data");
    }

    hash(data.data, data_hash_buffer);
    
    return data_hash;
}

static ErlNifFunc nif_funcs[] = {
  {"sequential_hash", 2, sequential_hash},
  {"hash", 1, hash256}  
};

ERL_NIF_INIT(hash, nif_funcs, NULL, NULL, NULL, NULL)
