/*XXXXXXXXXXXX
This is just a test; something I found on the web and thought
I'd try it to see if it gave different results from openssl
and agreed with the samples on the Amazon pages. It is not intended
to stay in the package.
 */

// From Erica Sadun, 15 March 2006
// cc signheader.c -w -o signheader  -lcurl -lcrypto

#include <stdio.h>
#include <string.h>
#include <openssl/hmac.h>
#include <openssl/evp.h>
#include <openssl/bio.h>
#include <curl/curl.h>

#define SECRETKEY "uV3F3YluFJax1cknvbcGwgjvx4QpvB+leU8dUj2o"
#define SLEN 1024

void doEncrypt(unsigned char *kString, char *sigString);

int
main(int argc, char *argv[])
{
    int riz, howlong;
    unsigned char headertext[SLEN], signature[SLEN];

    if (argc < 2)
    {
        printf("Usage: %s uri-encoded-headersn", argv[0]);
        exit(-1);
    }

    // Decode header
    sprintf(headertext, "%s", curl_unescape(argv[1], strlen(argv[1])));

    // Encrypt it
   doEncrypt(headertext, signature);

   printf("%s\n", signature);
}

void doEncrypt(unsigned char *kString, char *sigString)
{
    HMAC_CTX hctx;
    BIO *bio, *b64;
    char *sigptr;
    long siglen = -1;
    char *signature = NULL;
    unsigned int rizlen;
    unsigned char skey[SLEN], results[SLEN]; 

   // Initialize SHA1 encryption
    sprintf(skey, "%s", SECRETKEY);
    HMAC_CTX_init(&hctx);
    HMAC_Init(&hctx, skey, (int)strlen((char *)skey), EVP_sha1());

    // Encrypt
    HMAC(EVP_sha1(), skey, (int)strlen((char *)skey),
	(unsigned char *)kString, (int)strlen((char *)kString),
	results, &rizlen);

    // Base 64 Encode
    b64 = BIO_new(BIO_f_base64());
    bio = BIO_new(BIO_s_mem());
    BIO_set_flags(b64, BIO_FLAGS_BASE64_NO_NL);
    bio = BIO_push(b64, bio);
    BIO_write(bio, results, rizlen);
    BIO_flush(bio);

    siglen = BIO_get_mem_data(bio, &sigptr);
    signature = malloc(siglen+1);
    memcpy (signature, sigptr, siglen);
    signature[siglen] = '\0';
    sprintf(sigString, "%s", signature);

    // Clean up Encryption, Encoding
    BIO_free_all(bio);
    HMAC_CTX_cleanup(&hctx);
}
