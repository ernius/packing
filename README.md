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

* `hash.c`
Packing (sequential hashing) in C.
* `hash_nif` and `hash.erl` 
Erlang's Native Implemented Function (NIF) interface for C implementation `hash.c`.

### Packing

* `packing.erl` 
    * `parallel_packing/2` 
    Packs 64KB data using hashing C implementation in chunks of 32 Bytes. Chunks are computed in parallel, the maximum amount of spawn threads can be configured with `?MAX_SPAWN` macro (1 < MAX_SPAWN <= 2048).
    * `proof_of_custody/2` 
    Computes the proof of custody.
    * `validate_proof_of_custody/2`
    Validates a proof of custody.
    * `test/0` 
    Runs task steps by step as described in task's specification.

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
