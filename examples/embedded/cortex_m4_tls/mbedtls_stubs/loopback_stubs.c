/**
 * mbedTLS Loopback Stubs for QEMU Testing
 *
 * These stubs enable true loopback communication between a TLS client
 * and server running in the same process. Data written by one endpoint
 * can be read by the other.
 *
 * SPDX-License-Identifier: MIT
 */

#include <stddef.h>
#include <string.h>

/* Return codes */
#define MBEDTLS_ERR_SSL_WANT_READ  -0x6900
#define MBEDTLS_ERR_SSL_WANT_WRITE -0x6880

/* Ring buffer size - enough for Modbus ADU + TLS overhead */
#define RING_BUFFER_SIZE 512

/* Ring buffer structure */
typedef struct {
    unsigned char data[RING_BUFFER_SIZE];
    size_t head;  /* Write position */
    size_t tail;  /* Read position */
    size_t count; /* Bytes in buffer */
} ring_buffer_t;

/* Two ring buffers for bidirectional communication */
static ring_buffer_t client_to_server = {0};
static ring_buffer_t server_to_client = {0};

/* Connection identifiers */
#define CONN_CLIENT 0
#define CONN_SERVER 1
static int current_connection = CONN_CLIENT;
static int connection_count = 0;

/*
 * Explicit endpoint control - Ada code must call this before each
 * TLS operation to ensure reads/writes go to the correct buffer.
 */
void loopback_set_endpoint(int endpoint) {
    current_connection = endpoint;
}

/* Write to ring buffer */
static int ring_write(ring_buffer_t *rb, const unsigned char *data, size_t len) {
    size_t space = RING_BUFFER_SIZE - rb->count;
    if (len > space) {
        len = space;  /* Partial write */
    }
    for (size_t i = 0; i < len; i++) {
        rb->data[rb->head] = data[i];
        rb->head = (rb->head + 1) % RING_BUFFER_SIZE;
    }
    rb->count += len;
    return (int)len;
}

/* Read from ring buffer */
static int ring_read(ring_buffer_t *rb, unsigned char *data, size_t len) {
    if (rb->count == 0) {
        return MBEDTLS_ERR_SSL_WANT_READ;
    }
    if (len > rb->count) {
        len = rb->count;
    }
    for (size_t i = 0; i < len; i++) {
        data[i] = rb->data[rb->tail];
        rb->tail = (rb->tail + 1) % RING_BUFFER_SIZE;
    }
    rb->count -= len;
    return (int)len;
}

/* Reset buffers for new test */
void loopback_reset(void) {
    memset(&client_to_server, 0, sizeof(client_to_server));
    memset(&server_to_client, 0, sizeof(server_to_client));
    connection_count = 0;
    current_connection = CONN_CLIENT;
}

/* Get buffer sizes for debugging */
size_t loopback_c2s_count(void) { return client_to_server.count; }
size_t loopback_s2c_count(void) { return server_to_client.count; }

/*
 * Network stubs - these track which endpoint we're dealing with
 */

void mbedtls_net_init(void *ctx) {
    (void)ctx;
}

void mbedtls_net_free(void *ctx) {
    (void)ctx;
}

int mbedtls_net_connect(void *ctx, const char *host, const char *port, int proto) {
    (void)ctx; (void)host; (void)port; (void)proto;
    /* Client connects - mark context as client */
    current_connection = CONN_CLIENT;
    connection_count++;
    return 0;
}

int mbedtls_net_bind(void *ctx, const char *bind_ip, const char *port, int proto) {
    (void)ctx; (void)bind_ip; (void)port; (void)proto;
    return 0;
}

int mbedtls_net_accept(void *bind_ctx, void *client_ctx, void *client_ip,
                       size_t buf_size, size_t *ip_len) {
    (void)bind_ctx; (void)client_ctx; (void)client_ip; (void)buf_size;
    if (ip_len) *ip_len = 0;
    /* Server accepts - mark context as server */
    current_connection = CONN_SERVER;
    connection_count++;
    return 0;
}

int mbedtls_net_set_nonblock(void *ctx) { (void)ctx; return 0; }
int mbedtls_net_set_block(void *ctx) { (void)ctx; return 0; }

/*
 * These are the actual network send/recv - but TLS uses ssl_send/ssl_recv
 * which call the BIO callbacks. We'll handle this in ssl_write/ssl_read.
 */
int mbedtls_net_send(void *ctx, const unsigned char *buf, size_t len) {
    (void)ctx; (void)buf;
    return (int)len;
}

int mbedtls_net_recv(void *ctx, unsigned char *buf, size_t len) {
    (void)ctx; (void)buf; (void)len;
    return MBEDTLS_ERR_SSL_WANT_READ;
}

int mbedtls_net_recv_timeout(void *ctx, unsigned char *buf, size_t len, unsigned int timeout) {
    (void)ctx; (void)buf; (void)len; (void)timeout;
    return MBEDTLS_ERR_SSL_WANT_READ;
}

/*
 * SSL context stubs
 */

/* Store the endpoint type in the SSL context */
typedef struct {
    int endpoint;  /* 0 = client, 1 = server */
} ssl_context_stub_t;

static ssl_context_stub_t ssl_contexts[4];
static int ssl_ctx_count = 0;

void mbedtls_ssl_init(void *ssl) {
    if (ssl_ctx_count < 4) {
        ssl_contexts[ssl_ctx_count].endpoint = -1;
        ssl_ctx_count++;
    }
}

void mbedtls_ssl_free(void *ssl) {
    (void)ssl;
}

int mbedtls_ssl_setup(void *ssl, const void *conf) {
    (void)ssl; (void)conf;
    return 0;
}

void mbedtls_ssl_set_bio(void *ssl, void *p_bio, void *f_send, void *f_recv, void *f_recv_timeout) {
    (void)ssl; (void)p_bio; (void)f_send; (void)f_recv; (void)f_recv_timeout;
}

int mbedtls_ssl_set_hostname(void *ssl, const char *hostname) {
    (void)ssl; (void)hostname;
    return 0;
}

int mbedtls_ssl_handshake(void *ssl) {
    (void)ssl;
    return 0;  /* Handshake always succeeds */
}

/*
 * SSL Read/Write - the core loopback functionality
 */

int mbedtls_ssl_read(void *ssl, unsigned char *buf, size_t len) {
    (void)ssl;
    /* Client reads from server_to_client buffer */
    /* Server reads from client_to_server buffer */
    if (current_connection == CONN_CLIENT) {
        return ring_read(&server_to_client, buf, len);
    } else {
        return ring_read(&client_to_server, buf, len);
    }
}

int mbedtls_ssl_write(void *ssl, const unsigned char *buf, size_t len) {
    (void)ssl;
    /* Client writes to client_to_server buffer */
    /* Server writes to server_to_client buffer */
    if (current_connection == CONN_CLIENT) {
        return ring_write(&client_to_server, buf, len);
    } else {
        return ring_write(&server_to_client, buf, len);
    }
}

int mbedtls_ssl_close_notify(void *ssl) { (void)ssl; return 0; }
int mbedtls_ssl_session_reset(void *ssl) { (void)ssl; return 0; }

/*
 * SSL Config stubs
 */

void mbedtls_ssl_config_init(void *conf) { (void)conf; }
void mbedtls_ssl_config_free(void *conf) { (void)conf; }

int mbedtls_ssl_config_defaults(void *conf, int endpoint, int transport, int preset) {
    (void)conf; (void)transport; (void)preset;
    /* Track endpoint type: 0 = client, 1 = server */
    current_connection = (endpoint == 1) ? CONN_SERVER : CONN_CLIENT;
    return 0;
}

void mbedtls_ssl_conf_rng(void *conf, void *f_rng, void *p_rng) {
    (void)conf; (void)f_rng; (void)p_rng;
}

void mbedtls_ssl_conf_authmode(void *conf, int authmode) {
    (void)conf; (void)authmode;
}

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
    return 0;
}
int mbedtls_ssl_set_session(void *ssl, const void *session) {
    (void)ssl; (void)session;
    return 0;
}
void mbedtls_ssl_conf_session_tickets(void *conf, int use_tickets) {
    (void)conf; (void)use_tickets;
}
void mbedtls_ssl_conf_session_tickets_cb(void *conf, void *f_write,
                                          void *f_parse, void *p_ticket) {
    (void)conf; (void)f_write; (void)f_parse; (void)p_ticket;
}

/*
 * Entropy and RNG stubs
 */

void mbedtls_entropy_init(void *ctx) { (void)ctx; }
void mbedtls_entropy_free(void *ctx) { (void)ctx; }

int mbedtls_entropy_func(void *data, unsigned char *output, size_t len) {
    (void)data;
    for (size_t i = 0; i < len; i++) {
        output[i] = (unsigned char)(i * 17 + 42);
    }
    return 0;
}

int mbedtls_entropy_add_source(void *ctx, void *f_source, void *p_source,
                                size_t threshold, int strong) {
    (void)ctx; (void)f_source; (void)p_source;
    (void)threshold; (void)strong;
    return 0;
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

/*
 * X.509 and PK stubs
 */

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
