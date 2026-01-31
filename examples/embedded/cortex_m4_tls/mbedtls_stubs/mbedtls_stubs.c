/**
 * mbedTLS Stubs for QEMU Testing
 *
 * These are dummy implementations that allow the code to link and run
 * in QEMU for basic testing. They don't provide real TLS functionality.
 *
 * SPDX-License-Identifier: MIT
 */

#include <stddef.h>

/* Return codes */
#define MBEDTLS_ERR_SSL_WANT_READ  -0x6900
#define MBEDTLS_ERR_SSL_WANT_WRITE -0x6880

/* Stub implementations - all return success (0) or do nothing */

void mbedtls_net_init(void *ctx) { (void)ctx; }
void mbedtls_net_free(void *ctx) { (void)ctx; }
int mbedtls_net_connect(void *ctx, const char *host, const char *port, int proto) {
    (void)ctx; (void)host; (void)port; (void)proto;
    return 0;  /* Success */
}
int mbedtls_net_bind(void *ctx, const char *bind_ip, const char *port, int proto) {
    (void)ctx; (void)bind_ip; (void)port; (void)proto;
    return 0;  /* Success */
}
int mbedtls_net_accept(void *bind_ctx, void *client_ctx, void *client_ip,
                       size_t buf_size, size_t *ip_len) {
    (void)bind_ctx; (void)client_ctx; (void)client_ip; (void)buf_size;
    if (ip_len) *ip_len = 0;
    return 0;  /* Success - pretend client connected */
}
int mbedtls_net_set_nonblock(void *ctx) { (void)ctx; return 0; }
int mbedtls_net_set_block(void *ctx) { (void)ctx; return 0; }
int mbedtls_net_send(void *ctx, const unsigned char *buf, size_t len) {
    (void)ctx; (void)buf;
    return (int)len;  /* Pretend we sent everything */
}
int mbedtls_net_recv(void *ctx, unsigned char *buf, size_t len) {
    (void)ctx; (void)buf; (void)len;
    return MBEDTLS_ERR_SSL_WANT_READ;  /* No data available */
}
int mbedtls_net_recv_timeout(void *ctx, unsigned char *buf, size_t len, unsigned int timeout) {
    (void)ctx; (void)buf; (void)len; (void)timeout;
    return MBEDTLS_ERR_SSL_WANT_READ;
}

void mbedtls_ssl_init(void *ssl) { (void)ssl; }
void mbedtls_ssl_free(void *ssl) { (void)ssl; }
int mbedtls_ssl_setup(void *ssl, const void *conf) { (void)ssl; (void)conf; return 0; }
void mbedtls_ssl_set_bio(void *ssl, void *p_bio, void *f_send, void *f_recv, void *f_recv_timeout) {
    (void)ssl; (void)p_bio; (void)f_send; (void)f_recv; (void)f_recv_timeout;
}
int mbedtls_ssl_set_hostname(void *ssl, const char *hostname) { (void)ssl; (void)hostname; return 0; }
int mbedtls_ssl_handshake(void *ssl) { (void)ssl; return 0; }
/* Simulate a Modbus TCP response for testing */
static int read_call_count = 0;

/* Simulated Modbus TCP response: Read Holding Registers (FC 03) */
static const unsigned char fake_response[] = {
    0x00, 0x01,  /* Transaction ID */
    0x00, 0x00,  /* Protocol ID */
    0x00, 0x17,  /* Length: 23 bytes follow */
    0x01,        /* Unit ID */
    0x03,        /* Function Code: Read Holding Registers */
    0x14,        /* Byte count: 20 bytes (10 registers) */
    0x00, 0x0A, 0x00, 0x14, 0x00, 0x1E, 0x00, 0x28, 0x00, 0x32,  /* Registers 0-4 */
    0x00, 0x3C, 0x00, 0x46, 0x00, 0x50, 0x00, 0x5A, 0x00, 0x64   /* Registers 5-9 */
};

int mbedtls_ssl_read(void *ssl, unsigned char *buf, size_t len) {
    (void)ssl;
    read_call_count++;

    /* First call: return the fake response */
    if (read_call_count == 1) {
        size_t copy_len = len < sizeof(fake_response) ? len : sizeof(fake_response);
        for (size_t i = 0; i < copy_len; i++) {
            buf[i] = fake_response[i];
        }
        return (int)copy_len;
    }

    /* Subsequent calls: return remaining data or timeout */
    if (read_call_count == 2 && len > 0) {
        size_t offset = 7;  /* Already returned MBAP header */
        size_t remaining = sizeof(fake_response) - offset;
        size_t copy_len = len < remaining ? len : remaining;
        for (size_t i = 0; i < copy_len; i++) {
            buf[i] = fake_response[offset + i];
        }
        return (int)copy_len;
    }

    return MBEDTLS_ERR_SSL_WANT_READ;
}
int mbedtls_ssl_write(void *ssl, const unsigned char *buf, size_t len) {
    (void)ssl; (void)buf;
    return (int)len;
}
int mbedtls_ssl_close_notify(void *ssl) { (void)ssl; return 0; }
int mbedtls_ssl_session_reset(void *ssl) { (void)ssl; return 0; }

void mbedtls_ssl_config_init(void *conf) { (void)conf; }
void mbedtls_ssl_config_free(void *conf) { (void)conf; }
int mbedtls_ssl_config_defaults(void *conf, int endpoint, int transport, int preset) {
    (void)conf; (void)endpoint; (void)transport; (void)preset;
    return 0;
}
void mbedtls_ssl_conf_rng(void *conf, void *f_rng, void *p_rng) {
    (void)conf; (void)f_rng; (void)p_rng;
}
void mbedtls_ssl_conf_authmode(void *conf, int authmode) { (void)conf; (void)authmode; }
void mbedtls_ssl_conf_ca_chain(void *conf, void *ca_crt, void *ca_crl) {
    (void)conf; (void)ca_crt; (void)ca_crl;
}
int mbedtls_ssl_conf_own_cert(void *conf, void *own_crt, void *pk_key) {
    (void)conf; (void)own_crt; (void)pk_key;
    return 0;
}
void mbedtls_ssl_conf_read_timeout(void *conf, unsigned int timeout) {
    (void)conf; (void)timeout;
}
int mbedtls_ssl_conf_psk(void *conf, const unsigned char *psk, size_t psk_len,
                         const unsigned char *psk_identity, size_t psk_identity_len) {
    (void)conf; (void)psk; (void)psk_len;
    (void)psk_identity; (void)psk_identity_len;
    return 0;
}
void mbedtls_ssl_conf_psk_cb(void *conf, void *f_psk, void *p_psk) {
    (void)conf; (void)f_psk; (void)p_psk;
}
int mbedtls_ssl_set_hs_psk(void *ssl, const unsigned char *psk, size_t psk_len) {
    (void)ssl; (void)psk; (void)psk_len;
    return 0;
}

/* Session resumption stubs */
void mbedtls_ssl_session_init(void *session) { (void)session; }
void mbedtls_ssl_session_free(void *session) { (void)session; }
int mbedtls_ssl_get_session(const void *ssl, void *session) {
    (void)ssl; (void)session;
    return 0;  /* Success - pretend we saved the session */
}
int mbedtls_ssl_set_session(void *ssl, const void *session) {
    (void)ssl; (void)session;
    return 0;  /* Success - pretend we restored the session */
}
void mbedtls_ssl_conf_session_tickets(void *conf, int use_tickets) {
    (void)conf; (void)use_tickets;
}
void mbedtls_ssl_conf_session_tickets_cb(void *conf, void *f_write,
                                          void *f_parse, void *p_ticket) {
    (void)conf; (void)f_write; (void)f_parse; (void)p_ticket;
}

void mbedtls_entropy_init(void *ctx) { (void)ctx; }
void mbedtls_entropy_free(void *ctx) { (void)ctx; }
int mbedtls_entropy_func(void *data, unsigned char *output, size_t len) {
    (void)data;
    /* Fill with pseudo-random data */
    for (size_t i = 0; i < len; i++) {
        output[i] = (unsigned char)(i * 17 + 42);
    }
    return 0;
}
int mbedtls_entropy_add_source(void *ctx, void *f_source, void *p_source,
                                size_t threshold, int strong) {
    (void)ctx; (void)f_source; (void)p_source;
    (void)threshold; (void)strong;
    return 0;  /* Success */
}

void mbedtls_ctr_drbg_init(void *ctx) { (void)ctx; }
void mbedtls_ctr_drbg_free(void *ctx) { (void)ctx; }
int mbedtls_ctr_drbg_seed(void *ctx, void *f_entropy, void *p_entropy,
                          const unsigned char *custom, size_t len) {
    (void)ctx; (void)f_entropy; (void)p_entropy; (void)custom; (void)len;
    return 0;
}
int mbedtls_ctr_drbg_random(void *p_rng, unsigned char *output, size_t output_len) {
    (void)p_rng;
    for (size_t i = 0; i < output_len; i++) {
        output[i] = (unsigned char)(i * 31 + 7);
    }
    return 0;
}

void mbedtls_x509_crt_init(void *crt) { (void)crt; }
void mbedtls_x509_crt_free(void *crt) { (void)crt; }
int mbedtls_x509_crt_parse(void *chain, const unsigned char *buf, size_t buflen) {
    (void)chain; (void)buf; (void)buflen;
    return 0;
}
int mbedtls_x509_crt_parse_der(void *chain, const unsigned char *buf, size_t buflen) {
    (void)chain; (void)buf; (void)buflen;
    return 0;
}

void mbedtls_pk_init(void *ctx) { (void)ctx; }
void mbedtls_pk_free(void *ctx) { (void)ctx; }
int mbedtls_pk_parse_key(void *ctx, const unsigned char *key, size_t keylen,
                         const unsigned char *pwd, size_t pwdlen,
                         void *f_rng, void *p_rng) {
    (void)ctx; (void)key; (void)keylen; (void)pwd; (void)pwdlen;
    (void)f_rng; (void)p_rng;
    return 0;
}
