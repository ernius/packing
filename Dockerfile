FROM erlang:27.0-alpine

# Install required libs & tools
RUN apk add --no-cache libc-dev openssl libressl-dev gcc make
RUN mkdir /packing

# Copy our Erlang test application
COPY . packing

# And build the release
WORKDIR packing

# required env variables to run the Makefile
ENV CRYPTO_LIB=/usr/lib/libcrypto.so.3
ENV ERL_C_INCLUDE_PATH=/usr/local/lib/erlang

CMD ["/bin/sh"]

