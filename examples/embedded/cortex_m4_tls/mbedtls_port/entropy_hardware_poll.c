/**
 * Hardware entropy source for mbedTLS on Cortex-M
 *
 * NOTE: This is a WEAK implementation for testing only!
 * In production, use a proper hardware RNG or external entropy source.
 */

#include "mbedtls/entropy.h"

#if defined(MBEDTLS_ENTROPY_HARDWARE_ALT)

#include <string.h>

/* Simple counter-based "entropy" for testing - NOT SECURE! */
static unsigned int entropy_counter = 0x12345678;

int mbedtls_hardware_poll(void *data,
                          unsigned char *output, size_t len,
                          size_t *olen)
{
    (void)data;

    /* Generate pseudo-random bytes (NOT CRYPTOGRAPHICALLY SECURE) */
    for (size_t i = 0; i < len; i++) {
        entropy_counter = entropy_counter * 1103515245 + 12345;
        output[i] = (unsigned char)(entropy_counter >> 16);
    }

    *olen = len;
    return 0;
}

#endif /* MBEDTLS_ENTROPY_HARDWARE_ALT */
