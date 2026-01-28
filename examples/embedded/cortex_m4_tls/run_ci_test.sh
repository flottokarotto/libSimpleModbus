#!/bin/bash
# Run TLS test in QEMU with TLS server
# SPDX-License-Identifier: MIT
#
# This script:
# 1. Generates test certificates
# 2. Starts the TLS Modbus simulator
# 3. Runs the TLS client in QEMU
# 4. Checks for success

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CERT_DIR="/tmp/modbus_tls_certs"
PORT=8802

echo "=== Modbus TLS CI Test ==="
echo ""

# Generate test certificates
echo "Generating test certificates..."
mkdir -p "$CERT_DIR"

# Generate CA key and certificate
openssl req -x509 -newkey rsa:2048 \
    -keyout "$CERT_DIR/ca_key.pem" \
    -out "$CERT_DIR/ca_cert.pem" \
    -days 1 -nodes -subj "/CN=TestCA" 2>/dev/null

# Generate server key and CSR
openssl req -newkey rsa:2048 \
    -keyout "$CERT_DIR/server_key.pem" \
    -out "$CERT_DIR/server_csr.pem" \
    -nodes -subj "/CN=localhost" 2>/dev/null

# Sign server certificate with CA
openssl x509 -req \
    -in "$CERT_DIR/server_csr.pem" \
    -CA "$CERT_DIR/ca_cert.pem" \
    -CAkey "$CERT_DIR/ca_key.pem" \
    -CAcreateserial \
    -out "$CERT_DIR/server_cert.pem" \
    -days 1 2>/dev/null

echo "Certificates generated in $CERT_DIR"

# Start TLS Modbus server in background
echo "Starting TLS Modbus server on port $PORT..."
python3 "$SCRIPT_DIR/../../../tests/integration/modbus_tls_simulator.py" \
    --port $PORT \
    --cert "$CERT_DIR/server_cert.pem" \
    --key "$CERT_DIR/server_key.pem" \
    --ca "$CERT_DIR/ca_cert.pem" &
SERVER_PID=$!

# Give server time to start
sleep 2

# Check if server is running
if ! kill -0 $SERVER_PID 2>/dev/null; then
    echo "ERROR: TLS server failed to start"
    exit 1
fi

echo "TLS server running (PID: $SERVER_PID)"

# Run QEMU with networking
echo "Starting QEMU with TLS client..."
OUTPUT=$(timeout 30 qemu-system-arm -M lm3s6965evb -nographic -semihosting \
    -netdev user,id=net0,hostfwd=tcp::$PORT-10.0.2.2:$PORT \
    -net nic,netdev=net0 \
    -kernel "$SCRIPT_DIR/bin/main_tls_client.elf" 2>&1) || true

echo "QEMU output:"
echo "$OUTPUT"
echo ""

# Stop server
echo "Stopping TLS server..."
kill $SERVER_PID 2>/dev/null || true

# Check for success
if echo "$OUTPUT" | grep -q "TLS Test PASSED"; then
    echo ""
    echo "=== TEST PASSED ==="
    exit 0
else
    echo ""
    echo "=== TEST FAILED ==="
    exit 1
fi
