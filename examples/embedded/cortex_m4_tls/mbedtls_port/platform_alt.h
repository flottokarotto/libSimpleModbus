/**
 * Platform alternatives for embedded systems
 * Disables filesystem and time-related includes
 *
 * This file is force-included via -include before all source files
 * to override the default mbedTLS configuration for embedded targets.
 */

#ifndef PLATFORM_ALT_H
#define PLATFORM_ALT_H

#include <stdint.h>

/*
 * CRITICAL: Override mbedTLS configuration BEFORE headers are included.
 * These #undef/#define take effect when mbedtls/build_info.h is processed.
 */

/* Disable time functions - we provide our own ms_time via platform_time.c */
#undef MBEDTLS_HAVE_TIME
#undef MBEDTLS_HAVE_TIME_DATE
#define MBEDTLS_PLATFORM_MS_TIME_ALT  /* We provide mbedtls_ms_time() */

/* Disable platform entropy - we provide hardware RNG via entropy_hardware_poll.c */
#define MBEDTLS_NO_PLATFORM_ENTROPY

/* Disable filesystem I/O - not available on embedded */
#undef MBEDTLS_FS_IO

/* Disable features not needed for TLS client */
#undef MBEDTLS_SELF_TEST
#undef MBEDTLS_DEBUG_C
#undef MBEDTLS_ERROR_C
#undef MBEDTLS_SSL_SRV_C
#undef MBEDTLS_SSL_SESSION_TICKETS
#undef MBEDTLS_SSL_RENEGOTIATION
#undef MBEDTLS_SSL_DTLS_ANTI_REPLAY
#undef MBEDTLS_SSL_DTLS_HELLO_VERIFY
#undef MBEDTLS_PEM_PARSE_C
#undef MBEDTLS_BASE64_C
#undef MBEDTLS_ENTROPY_NV_SEED
#undef MBEDTLS_PSA_CRYPTO_C
#undef MBEDTLS_PSA_CRYPTO_STORAGE_C
#undef MBEDTLS_NET_C

/* Prevent inclusion of problematic headers */
#define _DIRENT_H_
#define _SYS_DIRENT_H_

/* Stub types for directory functions (won't be called with FS_IO disabled) */
typedef void DIR;
struct dirent { char d_name[256]; };

/* Stub functions that won't be called (FS_IO is disabled) */
static inline DIR *opendir(const char *p) { (void)p; return (DIR*)0; }
static inline struct dirent *readdir(DIR *d) { (void)d; return (struct dirent*)0; }
static inline int closedir(DIR *d) { (void)d; return 0; }

/* Time type declaration - implementation in platform_time.c */
typedef int64_t mbedtls_ms_time_t;
mbedtls_ms_time_t mbedtls_ms_time(void);

#endif /* PLATFORM_ALT_H */
