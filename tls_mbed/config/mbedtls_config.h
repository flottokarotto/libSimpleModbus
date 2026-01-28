/**
 * mbedTLS minimal configuration for Modbus TLS
 *
 * This configuration is optimized for:
 * - Modbus/TCP Security (Port 802, TLS 1.2+)
 * - Embedded MCU targets (Cortex-M4, ~256KB Flash, ~64KB RAM)
 * - Static memory allocation
 *
 * Memory footprint (approximate):
 * - Code: ~50-80 KB
 * - RAM: ~15-20 KB per TLS session
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef MBEDTLS_CONFIG_H
#define MBEDTLS_CONFIG_H

/*============================================================================*/
/*                              System Support                                 */
/*============================================================================*/

/* Use static memory allocation (no malloc/free) */
#define MBEDTLS_PLATFORM_C
#define MBEDTLS_PLATFORM_MEMORY
#define MBEDTLS_MEMORY_BUFFER_ALLOC_C

/* No file system access */
#undef MBEDTLS_FS_IO

/* No standard I/O */
#undef MBEDTLS_HAVE_TIME
#undef MBEDTLS_HAVE_TIME_DATE

/*============================================================================*/
/*                              Crypto Primitives                              */
/*============================================================================*/

/* AES cipher (required for TLS) */
#define MBEDTLS_AES_C
#define MBEDTLS_CIPHER_C
#define MBEDTLS_CIPHER_MODE_CBC
#define MBEDTLS_CIPHER_MODE_CTR

/* GCM mode (AEAD, recommended for TLS 1.2+) */
#define MBEDTLS_GCM_C

/* SHA hash functions */
#define MBEDTLS_SHA256_C
#define MBEDTLS_SHA384_C
#define MBEDTLS_SHA512_C
#define MBEDTLS_MD_C

/* HMAC */
#define MBEDTLS_MD_C

/*============================================================================*/
/*                              Key Exchange                                   */
/*============================================================================*/

/* RSA support (larger keys but widely used) */
#define MBEDTLS_RSA_C
#define MBEDTLS_BIGNUM_C
#define MBEDTLS_OID_C
#define MBEDTLS_PKCS1_V15
#define MBEDTLS_PKCS1_V21

/* ECDSA/ECDHE support (smaller keys, faster, recommended) */
#define MBEDTLS_ECP_C
#define MBEDTLS_ECDSA_C
#define MBEDTLS_ECDH_C

/* Supported elliptic curves */
#define MBEDTLS_ECP_DP_SECP256R1_ENABLED
#define MBEDTLS_ECP_DP_SECP384R1_ENABLED

/* Key exchange algorithms */
#define MBEDTLS_KEY_EXCHANGE_RSA_ENABLED
#define MBEDTLS_KEY_EXCHANGE_ECDHE_RSA_ENABLED
#define MBEDTLS_KEY_EXCHANGE_ECDHE_ECDSA_ENABLED

/*============================================================================*/
/*                              X.509 Certificates                             */
/*============================================================================*/

#define MBEDTLS_X509_CRT_PARSE_C
#define MBEDTLS_X509_USE_C
#define MBEDTLS_ASN1_PARSE_C
#define MBEDTLS_ASN1_WRITE_C
#define MBEDTLS_BASE64_C

/* Private key parsing */
#define MBEDTLS_PK_C
#define MBEDTLS_PK_PARSE_C

/* PEM format support (optional, can disable for DER-only) */
/* #define MBEDTLS_PEM_PARSE_C */

/*============================================================================*/
/*                              Random Number Generator                        */
/*============================================================================*/

#define MBEDTLS_CTR_DRBG_C
#define MBEDTLS_ENTROPY_C

/* Hardware entropy source (platform-specific, implement mbedtls_hardware_poll) */
#define MBEDTLS_ENTROPY_HARDWARE_ALT

/* Or use NV seed for entropy on platforms without hardware RNG */
/* #define MBEDTLS_ENTROPY_NV_SEED */

/*============================================================================*/
/*                              SSL/TLS Protocol                               */
/*============================================================================*/

#define MBEDTLS_SSL_TLS_C
#define MBEDTLS_SSL_CLI_C               /* Client mode */
/* #define MBEDTLS_SSL_SRV_C */         /* Server mode (enable if needed) */

/* TLS versions (1.2 minimum for Modbus Security) */
#define MBEDTLS_SSL_PROTO_TLS1_2
#define MBEDTLS_SSL_PROTO_TLS1_3        /* Optional: TLS 1.3 support */

/* Required cipher suites for Modbus Security */
#define MBEDTLS_SSL_CIPHERSUITES \
    MBEDTLS_TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256, \
    MBEDTLS_TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256, \
    MBEDTLS_TLS_RSA_WITH_AES_128_GCM_SHA256

/*============================================================================*/
/*                              Buffer Sizes                                   */
/*============================================================================*/

/* Maximum TLS record size (Modbus max PDU is 253 bytes, use 1KB for safety) */
#define MBEDTLS_SSL_IN_CONTENT_LEN      1024
#define MBEDTLS_SSL_OUT_CONTENT_LEN     1024

/* Maximum certificate size (2KB should be sufficient for most certs) */
#define MBEDTLS_SSL_MAX_CONTENT_LEN     2048

/* MPI (big number) maximum size */
#define MBEDTLS_MPI_MAX_SIZE            512

/*============================================================================*/
/*                              Optimizations                                  */
/*============================================================================*/

/* Use smaller code (slower but saves Flash) */
/* #define MBEDTLS_AES_ROM_TABLES */

/* Hardware acceleration (platform-specific) */
/* #define MBEDTLS_AES_ALT */
/* #define MBEDTLS_SHA256_ALT */

/*============================================================================*/
/*                              Debug (disable for release)                    */
/*============================================================================*/

/* #define MBEDTLS_DEBUG_C */
/* #define MBEDTLS_SSL_DEBUG_ALL */

/*============================================================================*/
/*                              Disabled Features                              */
/*============================================================================*/

/* No self-test functions (saves code space) */
#undef MBEDTLS_SELF_TEST

/* No deprecated functions */
#define MBEDTLS_DEPRECATED_REMOVED

/* No compression (not needed for Modbus, also security risk) */
#undef MBEDTLS_ZLIB_SUPPORT

/* No session tickets (simpler implementation) */
#undef MBEDTLS_SSL_SESSION_TICKETS

/* No renegotiation (security risk, not needed) */
#undef MBEDTLS_SSL_RENEGOTIATION

/*============================================================================*/
/*                              Include Check                                  */
/*============================================================================*/

#include "mbedtls/check_config.h"

#endif /* MBEDTLS_CONFIG_H */
