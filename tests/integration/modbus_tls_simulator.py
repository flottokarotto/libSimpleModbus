#!/usr/bin/env python3
"""
Modbus TLS Test Server for CI Testing

This server provides a TLS-secured Modbus TCP server for testing
the embedded TLS client in QEMU.

Usage:
    python3 modbus_tls_simulator.py --port 8802 \
        --cert server_cert.pem --key server_key.pem --ca ca_cert.pem

Requirements:
    pip install pymodbus

SPDX-License-Identifier: MIT
"""

import argparse
import asyncio
import logging
import ssl
import sys

from pymodbus.datastore import (
    ModbusSequentialDataBlock,
    ModbusServerContext,
    ModbusSlaveContext,
)
from pymodbus.server import StartAsyncTlsServer

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s"
)
log = logging.getLogger(__name__)


def create_datastore():
    """Create a Modbus datastore with test data."""
    # Create data blocks with sequential values for easy verification
    # Holding registers: 100 registers starting at address 0
    hr = ModbusSequentialDataBlock(0, [i * 10 for i in range(100)])

    # Input registers: 100 registers
    ir = ModbusSequentialDataBlock(0, [i * 5 for i in range(100)])

    # Coils: 100 coils (alternating True/False)
    co = ModbusSequentialDataBlock(0, [i % 2 == 0 for i in range(100)])

    # Discrete inputs: 100 inputs
    di = ModbusSequentialDataBlock(0, [i % 3 == 0 for i in range(100)])

    store = ModbusSlaveContext(
        di=di,  # Discrete Inputs (FC 02)
        co=co,  # Coils (FC 01, 05, 15)
        hr=hr,  # Holding Registers (FC 03, 06, 16)
        ir=ir,  # Input Registers (FC 04)
    )

    return ModbusServerContext(slaves=store, single=True)


def create_ssl_context(cert_file, key_file, ca_file, require_client_cert=False):
    """Create SSL context for TLS server."""
    ssl_context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)

    # Load server certificate and private key
    ssl_context.load_cert_chain(cert_file, key_file)

    # Load CA certificate for client verification (optional)
    if ca_file:
        ssl_context.load_verify_locations(ca_file)

    # Configure client certificate verification
    if require_client_cert:
        ssl_context.verify_mode = ssl.CERT_REQUIRED
    else:
        ssl_context.verify_mode = ssl.CERT_OPTIONAL

    # Set minimum TLS version (1.2 for Modbus Security)
    ssl_context.minimum_version = ssl.TLSVersion.TLSv1_2

    return ssl_context


async def run_server(port, cert_file, key_file, ca_file, require_client_cert):
    """Run the Modbus TLS server."""
    log.info(f"Starting Modbus TLS server on port {port}")

    # Create datastore
    context = create_datastore()
    log.info("Datastore created with test data")

    # Create SSL context
    ssl_context = create_ssl_context(
        cert_file, key_file, ca_file, require_client_cert
    )
    log.info("SSL context created")

    # Start server
    try:
        await StartAsyncTlsServer(
            context=context,
            address=("0.0.0.0", port),
            sslctx=ssl_context,
            allow_reuse_address=True,
        )
    except Exception as e:
        log.error(f"Server error: {e}")
        raise


def main():
    parser = argparse.ArgumentParser(
        description="Modbus TLS Test Server for CI"
    )
    parser.add_argument(
        "--port", type=int, default=8802,
        help="TCP port to listen on (default: 8802)"
    )
    parser.add_argument(
        "--cert", required=True,
        help="Server certificate file (PEM format)"
    )
    parser.add_argument(
        "--key", required=True,
        help="Server private key file (PEM format)"
    )
    parser.add_argument(
        "--ca", default=None,
        help="CA certificate for client verification (PEM format)"
    )
    parser.add_argument(
        "--require-client-cert", action="store_true",
        help="Require client certificate (mutual TLS)"
    )
    parser.add_argument(
        "--debug", action="store_true",
        help="Enable debug logging"
    )

    args = parser.parse_args()

    if args.debug:
        logging.getLogger().setLevel(logging.DEBUG)

    log.info("=" * 50)
    log.info("Modbus TLS Test Server")
    log.info("=" * 50)
    log.info(f"Port: {args.port}")
    log.info(f"Certificate: {args.cert}")
    log.info(f"Key: {args.key}")
    log.info(f"CA: {args.ca or 'None'}")
    log.info(f"Require client cert: {args.require_client_cert}")
    log.info("=" * 50)

    try:
        asyncio.run(
            run_server(
                args.port,
                args.cert,
                args.key,
                args.ca,
                args.require_client_cert
            )
        )
    except KeyboardInterrupt:
        log.info("Server stopped by user")
    except Exception as e:
        log.error(f"Failed to start server: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
