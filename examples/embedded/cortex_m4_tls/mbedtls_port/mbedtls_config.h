/**
 * Minimal mbedTLS configuration for Cortex-M + Modbus TLS
 *
 * Optimized for small footprint (~50-70KB Flash, ~20KB RAM)
 */

#ifndef MBEDTLS_CONFIG_H
#define MBEDTLS_CONFIG_H

/* System */
#define MBEDTLS_PLATFORM_C
#define MBEDTLS_PLATFORM_MEMORY
#define MBEDTLS_MEMORY_BUFFER_ALLOC_C
#define MBEDTLS_NO_PLATFORM_ENTROPY

/* Error strings disabled to save space */
#undef MBEDTLS_ERROR_C

/* Crypto - AES only */
#define MBEDTLS_AES_C
#define MBEDTLS_CIPHER_C
#define MBEDTLS_CIPHER_MODE_CBC
#define MBEDTLS_GCM_C

/* Hashing */
#define MBEDTLS_MD_C
#define MBEDTLS_SHA256_C

/* RSA (simpler than ECC, sufficient for testing) */
#define MBEDTLS_BIGNUM_C
#define MBEDTLS_OID_C
#define MBEDTLS_RSA_C
#define MBEDTLS_PKCS1_V15

/* X.509 */
#define MBEDTLS_X509_CRT_PARSE_C
#define MBEDTLS_X509_USE_C
#define MBEDTLS_ASN1_PARSE_C
#define MBEDTLS_PK_C
#define MBEDTLS_PK_PARSE_C

/* RNG */
#define MBEDTLS_CTR_DRBG_C
#define MBEDTLS_ENTROPY_C

/* SSL/TLS - Client only */
#define MBEDTLS_SSL_TLS_C
#define MBEDTLS_SSL_CLI_C
#define MBEDTLS_SSL_PROTO_TLS1_2
#define MBEDTLS_KEY_EXCHANGE_RSA_ENABLED

/* Network - we provide our own */
#undef MBEDTLS_NET_C

/* Buffer sizes - minimal for Modbus */
#define MBEDTLS_SSL_IN_CONTENT_LEN   1024
#define MBEDTLS_SSL_OUT_CONTENT_LEN  1024
#define MBEDTLS_SSL_MAX_CONTENT_LEN  1024
#define MBEDTLS_MPI_MAX_SIZE         256

/* Disable unused features */
#undef MBEDTLS_SELF_TEST
#undef MBEDTLS_DEBUG_C
#undef MBEDTLS_FS_IO
#undef MBEDTLS_HAVE_TIME
#undef MBEDTLS_HAVE_TIME_DATE
#undef MBEDTLS_PEM_PARSE_C
#undef MBEDTLS_BASE64_C
#undef MBEDTLS_SSL_SRV_C
#undef MBEDTLS_SSL_SESSION_TICKETS
#undef MBEDTLS_SSL_RENEGOTIATION
#undef MBEDTLS_SSL_DTLS_ANTI_REPLAY
#undef MBEDTLS_SSL_DTLS_HELLO_VERIFY

/* Explicitly disable problematic features for embedded */
#undef MBEDTLS_X509_CRT_PARSE_PATH
#undef MBEDTLS_ENTROPY_NV_SEED
#undef MBEDTLS_PSA_CRYPTO_C
#undef MBEDTLS_PSA_CRYPTO_STORAGE_C
#undef MBEDTLS_HAVE_TIME

/* Provide stub for ms_time - we don't need accurate timing */
#define MBEDTLS_PLATFORM_MS_TIME_ALT

/* Version */
#define MBEDTLS_VERSION_C

#include "mbedtls/check_config.h"

#endif /* MBEDTLS_CONFIG_H */
