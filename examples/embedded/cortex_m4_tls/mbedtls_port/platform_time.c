/**
 * Platform time implementation for embedded systems
 *
 * Provides a simple counter-based time function.
 * This is sufficient for TLS client operation where
 * exact time is not critical.
 */

#include "mbedtls/platform_time.h"
#include <stdint.h>

/* Simple incrementing counter for "time" */
static volatile uint32_t tick_counter = 0;

mbedtls_ms_time_t mbedtls_ms_time(void)
{
    /* Return incrementing counter as milliseconds */
    /* In real embedded code, this would read a hardware timer */
    tick_counter += 10;
    return (mbedtls_ms_time_t)tick_counter;
}

/* Also provide mbedtls_time if needed */
#if defined(MBEDTLS_HAVE_TIME)
#include <time.h>
time_t mbedtls_time(time_t *timer)
{
    time_t t = (time_t)(tick_counter / 1000);
    if (timer != NULL) {
        *timer = t;
    }
    return t;
}
#endif
