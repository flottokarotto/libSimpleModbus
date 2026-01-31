# Cortex-M4 with TLS

Modbus over TLS examples for ARM Cortex-M using mbedTLS.

This demonstrates secure Modbus communication on embedded systems, following the Modbus/TCP Security specification.

## Quick Start (QEMU)

```bash
# Install the ARM cross-compiler
alr toolchain --select gnat_arm_elf

# Build and run the loopback test
cd examples/embedded/cortex_m4_tls
alr build
qemu-system-arm -M lm3s6965evb -nographic -semihosting -kernel bin/main_loopback_tls.elf
```

Expected output:
```
=== Modbus TLS Loopback Test ===
TLS handshake simulation... OK
Encrypt/decrypt test... OK
LOOPBACK TEST PASSED
```

Exit QEMU with `Ctrl+A`, then `X`.

## Available Programs

| Program | Description |
|---------|-------------|
| `main_loopback_tls` | TLS handshake and encryption self-test |
| `main_tls_client` | TLS client connecting to a Modbus server |
| `main_tls_server` | TLS server accepting Modbus connections |

## Build Modes

```bash
# Loopback mode with mbedTLS stubs (for CI, smaller binary)
alr exec -- gprbuild -P cortex_m4_tls.gpr -XMBEDTLS_MODE=loopback

# Full mbedTLS (for real TLS operations)
alr exec -- gprbuild -P cortex_m4_tls.gpr -XMBEDTLS_MODE=full
```

## Memory Usage

| Mode | Flash | RAM |
|------|-------|-----|
| Loopback with stubs | ~15 KB | ~12 KB |
| Full mbedTLS | ~80 KB | ~25 KB |

The full mbedTLS build requires a microcontroller with at least 128 KB Flash and 32 KB RAM.

## Test Certificates

The example includes placeholder certificates in `src/test_certificates.ads`. For real deployments, generate proper certificates:

```bash
# Generate a CA
openssl req -x509 -newkey rsa:2048 -keyout ca_key.pem -out ca_cert.pem -days 365 -nodes -subj "/CN=TestCA"

# Convert to DER format for embedding
openssl x509 -in ca_cert.pem -outform DER -out ca_cert.der
```
