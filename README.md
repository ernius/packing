# Packing task

```
.
├── Dockerfile
├── hash.c
├── hash.erl
├── hash_nif.c
├── Makefile
├── packing.erl
└── README.md
```

## Description

### Hashing

* [`hash.c`](https://github.com/ernius/packing/blob/master/hash.c)
Sequential hashing iteration in C.
    * [`hash_data_chunk`](https://github.com/ernius/packing/blob/master/hash.c#L19) procedure implements sequential hashing interation.
    * [`hash`](https://github.com/ernius/packing/blob/master/hash.c#L92) procedure does a simple sha256 hash, just to validate library usage against erlang's crypto library.
* [`hash_nif.c`](https://github.com/ernius/packing/blob/master/hash_nif.c) and [`hash.erl`](https://github.com/ernius/packing/blob/master/hash.erl) 
Erlang's Native Implemented Function (NIF) interface for C implementation in `hash.c`.

### Packing

* [`packing.erl`](https://github.com/ernius/packing/blob/master/packing.erl) 
    * [`parallel_packing/2`](https://github.com/ernius/packing/blob/master/packing.erl#L136) 
    Packs 64KB data using hashing C implementation in chunks of 32 Bytes. Chunks are computed in parallel, the maximum amount of spawn threads can be configured with `?MAX_SPAWN` macro (1 < MAX_SPAWN <= 2048).
    * [`sequential_packing/2`](https://github.com/ernius/packing/blob/master/packing.erl#L163)  first sequential version fo the packing algorithm, left just to validate and compare times.
    * [`proof_of_custody/2`](https://github.com/ernius/packing/blob/master/packing.erl#L181)
    Computes the proof of custody.
    * [`validate_proof_of_custody/2`](https://github.com/ernius/packing/blob/master/packing.erl#L196)
    Validates a proof of custody.
    * [`test/0`](https://github.com/ernius/packing/blob/master/packing.erl#L37) 
    Runs task steps by step as described in task's specification.
    * [`test_sha256/0`](https://github.com/ernius/packing/blob/master/packing.erl#L86) 
    Tests nif mechanism and validates agains crypto library.

## Running

Containarized solution is provided to easily run the solution.

1. Create container

```
make docker_build
```

2. Run containarized solution

```
make docker_test
```

* Run containarized erlang interpreter with loaded solution

```
make docker_erl
```
