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
    * [`hash_data_chunk`](https://github.com/ernius/packing/blob/master/hash.c#L19) procedure implements sequential hashing iteration.
    * [`hash`](https://github.com/ernius/packing/blob/master/hash.c#L92) procedure does a simple sha256 hash, just to validate library usage against Erlang's crypto library.
* [`hash_nif.c`](https://github.com/ernius/packing/blob/master/hash_nif.c) and [`hash.erl`](https://github.com/ernius/packing/blob/master/hash.erl) 
Erlang's Native Implemented Function (NIF) interface for C implementation in `hash.c`.

### Packing

* [`packing.erl`](https://github.com/ernius/packing/blob/master/packing.erl) 
    * [`parallel_packing/2`](https://github.com/ernius/packing/blob/master/packing.erl#L136) 
    Packs 64KB data using hashing C implementation in chunks of 32 Bytes. Chunks are computed in parallel, the maximum amount of spawn threads can be configured with `?MAX_SPAWN` macro (1 < MAX_SPAWN <= 2048).
    * [`sequential_packing/2`](https://github.com/ernius/packing/blob/master/packing.erl#L163)  first sequential version of the packing algorithm, left just to validate and compare times.
    * [`proof_of_custody/2`](https://github.com/ernius/packing/blob/master/packing.erl#L181)
    Computes the proof of custody.
    * [`validate_proof_of_custody/2`](https://github.com/ernius/packing/blob/master/packing.erl#L196)
    Validates a proof of custody.
    * [`test/0`](https://github.com/ernius/packing/blob/master/packing.erl#L37) 
    Runs task steps by step as described in task's specification.
    * [`test_sha256/0`](https://github.com/ernius/packing/blob/master/packing.erl#L86) 
    Tests NIF mechanism and validates against crypto library.

## Running

Containerized solution is provided to easily run the solution.

1. Create container

```
make docker_build
```

2. Run containerized solution

```
make docker_test
```

Test Output example:

```console
ernius@ernius-82lm:~/Documents/packing$ make docker_test 
docker run -it -v .:/packing erlang-packing sh -c "make test"
gcc -o hash_nif.so -lcrypto -I/usr/local/lib/erlang -fpic -shared hash.c hash_nif.c 
erlc  +bin_opt_inf hash.erl
erlc  +bin_opt_inf packing.erl #bin_opt_inf gives binaries use performance tips & warns
LD_PRELOAD=/usr/lib/libcrypto.so.3 erl -noshell -eval "packing:test(),erlang:halt()."
generating random data and miner addresses ... 
miner address: <<"4E3AA3CA8536D5C79DE90FACF4ECB424F9355CE461CA9B958CA9C8D07727BA7E">>
starting packing in parallel ...
finish packing in parallel in: 10.996323 seconds
generating random block hash ...
block hash: <<"0A25D8B020A3132BDCC853364F2C3DF9344D1958C2F7EB974C62772DA38D73FC">>
generating proof of custody ...
proof of custody: [<<"7E1545FDC66AEF350B4ECFF5714DD9151AF3B281A4DE522E399F757610715F6E">>,
                   <<"00610668B4580AD88721F0CD7F4B0D6695C48CAB99ED0668C1403589869240B1">>,
                   <<"1ED6EF04EE04057C0B43AFE22F13E650A286A011C8A6C0F25CB3BD5445E85439">>,
                   <<"31908263DE550FFD705A6DD4E20C701E82A9D78CB644254FC60F5DBF1FED0B22">>,
                   <<"07AB17D870B84AFBE2483B1F6FCD3A1D6613B7B3BF738FC9725468419BE1AEAC">>,
                   <<"80260749C4E281C99DFBD1158B96A0EA5BD776C1A081AF0421E9C2665E45EC1A">>,
                   <<"987ACE78FE81639996007CAAC62B617DE57B573A871A3119C2A6BCAEAAA22810">>,
                   <<"015D4A01827D7B0BC075B767040218D1E0BF984C205C36D28E966B5B8F6D2A49">>,
                   <<"51339DD8441737BF9D08AE86FA748FCE346976FB5C3C4D0184ABDA5A95374264">>,
                   <<"3EC9C557E85B6B3C78E5890D0A15BDDB7BC1D821426B7317FFEF2C5BC3A1C277">>]
validating proof of custody ...
Validation ended OK
```


* Run containerized Erlang interpreter with loaded solution

```
make docker_erl
```
