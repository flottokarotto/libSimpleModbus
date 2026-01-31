#!/usr/bin/env python3
"""Modbus TCP over TLS Server for AdaModbus Integration Tests

A pymodbus-based Modbus TCP server with TLS encryption for testing
the adamodbus_tls transport layer.

Copyright (c) 2026 Florian Fischer
SPDX-License-Identifier: MIT

Requirements:
    pip install pymodbus

Usage:
    python tls_modbus_server.py --cert server.crt --key server.key [--ca ca.crt]

Default port: 8802 (non-privileged alternative to 802)
"""

import argparse
import logging
import ssl
import sys
import os

try:
    from pymodbus.datastore import (
        ModbusSequentialDataBlock,
        ModbusDeviceContext,
        ModbusServerContext,
    )
    from pymodbus.server import StartTlsServer
    from pymodbus import __version__ as pymodbus_version
except ImportError as e:
    print(f"Error: pymodbus is not installed or TLS support missing.")
    print("Install it with: pip install pymodbus")
    sys.exit(1)


# Test data - same as modbus_simulator.py
HOLDING_REGISTERS_INITIAL = [
    0x0000,  # dummy for 1-based addressing
    # Address 0-9: Sequential pattern
    0x0000, 0x0064, 0x00C8, 0x012C, 0x0190,  # 0, 100, 200, 300, 400
    0x01F4, 0x0258, 0x02BC, 0x0320, 0x0384,  # 500, 600, 700, 800, 900
    # Address 10-19: Test values for verification
    0x1234, 0x5678, 0x9ABC, 0xDEF0, 0xAAAA,
    0x5555, 0xFF00, 0x00FF, 0x8000, 0x7FFF,
] + [0] * 180  # Pad to 201 registers

INPUT_REGISTERS = [0] + [i + 1000 for i in range(200)]
COILS_INITIAL = [False] + [i % 2 == 0 for i in range(100)]
DISCRETE_INPUTS = [False] + [i % 3 == 0 for i in range(100)]


def create_datastore(unit_id: int) -> ModbusServerContext:
    """Create and initialize the Modbus data store."""
    coils = ModbusSequentialDataBlock(0, COILS_INITIAL)
    discrete_inputs = ModbusSequentialDataBlock(0, DISCRETE_INPUTS)
    holding_registers = ModbusSequentialDataBlock(0, HOLDING_REGISTERS_INITIAL.copy())
    input_registers = ModbusSequentialDataBlock(0, INPUT_REGISTERS)

    store = ModbusDeviceContext(
        di=discrete_inputs,
        co=coils,
        hr=holding_registers,
        ir=input_registers,
    )

    context = ModbusServerContext(devices={unit_id: store}, single=False)
    return context


def run_tls_server(
    port: int,
    cert_file: str,
    key_file: str,
    ca_file: str = None,
    unit_id: int = 1,
    require_client_cert: bool = False,
):
    """Start the Modbus TCP over TLS server."""
    log = logging.getLogger(__name__)

    log.info(f"Starting Modbus TLS Server on port {port}")
    log.info(f"Certificate: {cert_file}")
    log.info(f"Private key: {key_file}")
    if ca_file:
        log.info(f"CA certificate: {ca_file}")
    log.info(f"Client cert required: {require_client_cert}")
    log.info(f"Unit ID: {unit_id}")
    log.info(f"pymodbus version: {pymodbus_version}")
    log.info("-" * 50)

    # Verify certificate files exist
    for f, name in [(cert_file, "Certificate"), (key_file, "Private key")]:
        if not os.path.exists(f):
            log.error(f"{name} file not found: {f}")
            sys.exit(1)

    if ca_file and not os.path.exists(ca_file):
        log.error(f"CA certificate file not found: {ca_file}")
        sys.exit(1)

    context = create_datastore(unit_id)

    # Create SSL context
    ssl_context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    ssl_context.load_cert_chain(certfile=cert_file, keyfile=key_file)

    if ca_file:
        ssl_context.load_verify_locations(cafile=ca_file)

    if require_client_cert:
        ssl_context.verify_mode = ssl.CERT_REQUIRED
    else:
        ssl_context.verify_mode = ssl.CERT_NONE

    try:
        StartTlsServer(
            context=context,
            address=("0.0.0.0", port),
            sslctx=ssl_context,
        )
    except KeyboardInterrupt:
        log.info("Server stopped by user")
    except Exception as e:
        log.error(f"Server error: {e}")
        raise


def main():
    parser = argparse.ArgumentParser(
        description="Modbus TCP over TLS Server for Integration Tests"
    )
    parser.add_argument(
        "--port", "-p",
        type=int,
        default=8802,
        help="TCP port to listen on (default: 8802)"
    )
    parser.add_argument(
        "--cert", "-c",
        required=True,
        help="Server certificate file (PEM format)"
    )
    parser.add_argument(
        "--key", "-k",
        required=True,
        help="Server private key file (PEM format)"
    )
    parser.add_argument(
        "--ca",
        help="CA certificate for client verification (PEM format)"
    )
    parser.add_argument(
        "--require-client-cert",
        action="store_true",
        help="Require client certificate (mutual TLS)"
    )
    parser.add_argument(
        "--unit-id", "-u",
        type=int,
        default=1,
        help="Modbus unit/slave ID (default: 1)"
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Enable verbose logging"
    )

    args = parser.parse_args()

    log_level = logging.DEBUG if args.verbose else logging.INFO
    logging.basicConfig(
        level=log_level,
        format='%(asctime)s - %(levelname)s - %(message)s'
    )

    run_tls_server(
        port=args.port,
        cert_file=args.cert,
        key_file=args.key,
        ca_file=args.ca,
        unit_id=args.unit_id,
        require_client_cert=args.require_client_cert,
    )


if __name__ == "__main__":
    main()
