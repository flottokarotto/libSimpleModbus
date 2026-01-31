#!/bin/bash
# Generate test certificates for TLS integration tests
#
# Creates:
#   - ca.key, ca.crt: Root CA
#   - server.key, server.crt: Server certificate
#   - client.key, client.crt: Client certificate (for mutual TLS)
#
# All certificates are self-signed and valid for localhost testing.
#
# SPDX-License-Identifier: MIT

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

DAYS=365
KEY_SIZE=2048

echo "=== Generating Test Certificates for TLS Integration Tests ==="
echo ""

# Clean up old certificates
rm -f *.key *.crt *.csr *.srl

# 1. Generate CA key and certificate
echo "1. Generating CA certificate..."
openssl genrsa -out ca.key $KEY_SIZE 2>/dev/null
openssl req -new -x509 -days $DAYS -key ca.key -out ca.crt \
    -subj "/CN=AdaModbus Test CA/O=AdaModbus/C=DE" 2>/dev/null
echo "   Created: ca.key, ca.crt"

# 2. Generate server key and CSR
echo "2. Generating server certificate..."
openssl genrsa -out server.key $KEY_SIZE 2>/dev/null
openssl req -new -key server.key -out server.csr \
    -subj "/CN=localhost/O=AdaModbus/C=DE" 2>/dev/null

# Create server certificate with SAN for localhost
cat > server_ext.cnf << EOF
authorityKeyIdentifier=keyid,issuer
basicConstraints=CA:FALSE
keyUsage = digitalSignature, keyEncipherment
extendedKeyUsage = serverAuth
subjectAltName = @alt_names

[alt_names]
DNS.1 = localhost
DNS.2 = *.localhost
IP.1 = 127.0.0.1
IP.2 = ::1
EOF

openssl x509 -req -in server.csr -CA ca.crt -CAkey ca.key -CAcreateserial \
    -out server.crt -days $DAYS -extfile server_ext.cnf 2>/dev/null
rm -f server.csr server_ext.cnf
echo "   Created: server.key, server.crt"

# 3. Generate client key and certificate (for mutual TLS)
echo "3. Generating client certificate..."
openssl genrsa -out client.key $KEY_SIZE 2>/dev/null
openssl req -new -key client.key -out client.csr \
    -subj "/CN=AdaModbus Test Client/O=AdaModbus/C=DE" 2>/dev/null

cat > client_ext.cnf << EOF
authorityKeyIdentifier=keyid,issuer
basicConstraints=CA:FALSE
keyUsage = digitalSignature
extendedKeyUsage = clientAuth
EOF

openssl x509 -req -in client.csr -CA ca.crt -CAkey ca.key -CAcreateserial \
    -out client.crt -days $DAYS -extfile client_ext.cnf 2>/dev/null
rm -f client.csr client_ext.cnf ca.srl
echo "   Created: client.key, client.crt"

echo ""
echo "=== Certificate generation complete ==="
echo ""
echo "Files created:"
ls -la *.key *.crt 2>/dev/null || true
echo ""
echo "To verify certificates:"
echo "  openssl verify -CAfile ca.crt server.crt"
echo "  openssl verify -CAfile ca.crt client.crt"
