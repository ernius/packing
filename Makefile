# erlang instalation path
ERL_C_INCLUDE_PATH ?= /home/ernius/.asdf/installs/erlang/27.0
# libssl-dev shared library location
CRYPTO_LIB ?= /usr/lib/x86_64-linux-gnu/libcrypto.so.3

# docker image name
DOCKER_IMAGE ?= erlang-packing

test: packing.beam
	LD_PRELOAD=${CRYPTO_LIB} erl -noshell -eval "packing:test(),erlang:halt()."

console: packing.beam
	LD_PRELOAD=${CRYPTO_LIB} erl

packing.beam: packing.erl hash.beam
	erlc  +bin_opt_inf packing.erl #bin_opt_inf gives binaries use performance tips & warns

hash.beam: hash.erl hash_nif.so
	erlc  +bin_opt_inf hash.erl

hash_nif.so: hash_nif.c hash.c
	gcc -o hash_nif.so -lcrypto -I${ERL_C_INCLUDE_PATH} -fpic -shared hash.c hash_nif.c 

clean:
	rm -f *.beam *.so *~ *.dump 

docker_build:
	make clean
	docker build -t ${DOCKER_IMAGE} .

docker_terminal: 
	docker run -it -v .:/packing ${DOCKER_IMAGE} sh 

docker_erl:
	docker run -it -v .:/packing ${DOCKER_IMAGE} sh -c "make console"

docker_test:
	docker run -it -v .:/packing ${DOCKER_IMAGE} sh -c "make test"

