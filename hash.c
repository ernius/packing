#include <stdio.h>
#include <stdlib.h>
#include <openssl/sha.h>	/* OpenSSL headers for hashing */
#include <openssl/evp.h>

#define MINER_ADDRESS_SIZE 32 /* in Bytes */
#define CHUNK_SIZE 32

// OpenSSL engine implementation
#define OPENSSL_ENGINE NULL

/**
 * Returns the SHA256 value of miner address and data
 *
 * @param mining_address 32 bytes mining addresss
 * @param data_chunk 32 bytes of data 
 * @returns data_chunck_hash resuting mining address and data hash
 */
void hash_data_chunk(unsigned char * mining_address, unsigned char * data_chunk, unsigned char * data_chunk_hash)
{
    EVP_MD_CTX *mdCtx = EVP_MD_CTX_new();
    unsigned int mdLen, i;

    if (!EVP_DigestInit_ex(mdCtx, EVP_sha256(), OPENSSL_ENGINE))
    {
        printf("Message digest initialization failed.\n");
        EVP_MD_CTX_free(mdCtx);
        exit(EXIT_FAILURE);
    }

    // Hashes cnt bytes of data at d into the digest context mdCtx
    if (!EVP_DigestUpdate(mdCtx, mining_address, MINER_ADDRESS_SIZE))
    {
        printf("Message digest update of mining address failed.\n");
        EVP_MD_CTX_free(mdCtx);
        exit(EXIT_FAILURE);
    }
    // Hashes cnt bytes of data at d into the digest context mdCtx
    if (!EVP_DigestUpdate(mdCtx, data_chunk, CHUNK_SIZE))
    {
        printf("Message digest update of data failed.\n");
        EVP_MD_CTX_free(mdCtx);
        exit(EXIT_FAILURE);
    }

    // writes result
    if (!EVP_DigestFinal_ex(mdCtx, data_chunk_hash, &mdLen))
    {
        printf("Message digest finalization failed.\n");
        EVP_MD_CTX_free(mdCtx);
        exit(EXIT_FAILURE);
    }

    // reinits hashing context
    if (!EVP_DigestInit_ex(mdCtx, EVP_sha256(), OPENSSL_ENGINE))
    {
        printf("Message digest initialization failed.\n");
        EVP_MD_CTX_free(mdCtx);
        exit(EXIT_FAILURE);
    }

    for (i = 0; i < 1000000; i++){
      if (!EVP_DigestUpdate(mdCtx, data_chunk_hash, CHUNK_SIZE))
	{
	  printf("Message digest sequential update iteration of data.\n");
	  EVP_MD_CTX_free(mdCtx);
	  exit(EXIT_FAILURE);
	}
    }

    // writes result
    if (!EVP_DigestFinal_ex(mdCtx, data_chunk_hash, &mdLen))
    {
        printf("Message digest finalization failed.\n");
        EVP_MD_CTX_free(mdCtx);
        exit(EXIT_FAILURE);
    }

    EVP_MD_CTX_free(mdCtx);
    /* printf("DEBUG: Digest is: "); */
    /* for (i = 0; i < mdLen; i++) */
    /*     printf("%02x", data_chunk_hash[i]); */
    /* printf("\n"); */
}

/**
 * Returns SHA256 to check against erlang crypto library
 *
 * @param data 32 bytes of data 
 * @returns returns 32 bytes hash
 */
void hash(unsigned char * data, unsigned char * data_hash)
{
    EVP_MD_CTX *mdCtx = EVP_MD_CTX_new();
    unsigned int mdLen, i;

    if (!EVP_DigestInit_ex(mdCtx, EVP_sha256(), OPENSSL_ENGINE))
    {
        printf("Message digest initialization failed.\n");
        EVP_MD_CTX_free(mdCtx);
        exit(EXIT_FAILURE);
    }

    // Hashes cnt bytes of data at d into the digest context mdCtx
    if (!EVP_DigestUpdate(mdCtx, data, CHUNK_SIZE))
    {
        printf("Message digest update of data failed.\n");
        EVP_MD_CTX_free(mdCtx);
        exit(EXIT_FAILURE);
    }

    // writes result
    if (!EVP_DigestFinal_ex(mdCtx, data_hash, &mdLen))
    {
        printf("Message digest finalization failed.\n");
        EVP_MD_CTX_free(mdCtx);
        exit(EXIT_FAILURE);
    }

    EVP_MD_CTX_free(mdCtx);
    /* printf("DEBUG: Digest is: "); */
    /* for (i = 0; i < mdLen; i++) */
    /*     printf("%02x", data_chunk_hash[i]); */
    /* printf("\n"); */
}
