/**
 * Minimal mbedTLS configuration for Cortex-M + Modbus TLS
 * Based on tls_mbed/config/mbedtls_config.h
 */

#ifndef MBEDTLS_CONFIG_H
#define MBEDTLS_CONFIG_H

/* System */
#define MBEDTLS_PLATFORM_C
#define MBEDTLS_PLATFORM_MEMORY
#define MBEDTLS_MEMORY_BUFFER_ALLOC_C
#define MBEDTLS_NO_PLATFORM_ENTROPY

/* Crypto */
#define MBEDTLS_AES_C
#define MBEDTLS_CIPHER_C
#define MBEDTLS_CIPHER_MODE_CBC
#define MBEDTLS_GCM_C
#define MBEDTLS_SHA256_C
#define MBEDTLS_MD_C

/* Asymmetric */
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

/* SSL/TLS */
#define MBEDTLS_SSL_TLS_C
#define MBEDTLS_SSL_CLI_C
#define MBEDTLS_SSL_PROTO_TLS1_2
#define MBEDTLS_KEY_EXCHANGE_RSA_ENABLED

/* Buffer sizes */
#define MBEDTLS_SSL_IN_CONTENT_LEN  1024
#define MBEDTLS_SSL_OUT_CONTENT_LEN 1024
#define MBEDTLS_MPI_MAX_SIZE        256

/* Disable unused */
#undef MBEDTLS_SELF_TEST
#undef MBEDTLS_DEBUG_C
#undef MBEDTLS_FS_IO
#undef MBEDTLS_NET_C

#include "mbedtls/check_config.h"

#endif
