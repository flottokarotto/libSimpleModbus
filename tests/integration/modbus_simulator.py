#!/usr/bin/env python3
"""Modbus TCP Simulator for AdaModbus Integration Tests

A pymodbus-based Modbus TCP server with predefined register values for testing
all function codes supported by the AdaModbus library.

Copyright (c) 2026 Florian Fischer
SPDX-License-Identifier: MIT

Requirements:
    pip install pymodbus

Usage:
    python modbus_simulator.py [--port PORT] [--unit-id UNIT_ID] [--verbose]

Default:
    Port: 5020 (non-privileged, avoids conflicts with real Modbus servers)
    Unit ID: 1
"""

import argparse
import logging
import sys
import threading
import time
from typing import Optional

try:
    from pymodbus.datastore import (
        ModbusSequentialDataBlock,
        ModbusSlaveContext,
        ModbusServerContext,
    )
    from pymodbus.server import StartTcpServer
    from pymodbus.device import ModbusDeviceIdentification
    from pymodbus.version import version as pymodbus_version
except ImportError:
    print("Error: pymodbus is not installed.")
    print("Install it with: pip install pymodbus")
    sys.exit(1)


# Test data configuration
# These values are used to verify correct read/write operations

# Coils (FC 01/05/15) - Address 0-99
# Pattern: alternating on/off starting at address 0
COILS_INITIAL = [i % 2 == 0 for i in range(100)]

# Discrete Inputs (FC 02) - Address 0-99
# Pattern: every 3rd bit is set
DISCRETE_INPUTS = [i % 3 == 0 for i in range(100)]

# Holding Registers (FC 03/06/16/22/23) - Address 0-199
# Pattern: address * 100 (e.g., addr 0 = 0, addr 1 = 100, addr 10 = 1000)
# Special values at specific addresses for test verification
HOLDING_REGISTERS_INITIAL = [
    # Address 0-9: Sequential pattern
    0x0000, 0x0064, 0x00C8, 0x012C, 0x0190,  # 0, 100, 200, 300, 400
    0x01F4, 0x0258, 0x02BC, 0x0320, 0x0384,  # 500, 600, 700, 800, 900
    # Address 10-19: Test values for verification
    0x1234, 0x5678, 0x9ABC, 0xDEF0, 0xAAAA,
    0x5555, 0xFF00, 0x00FF, 0x8000, 0x7FFF,
    # Address 20-29: SunSpec-like values (Model 1 Common)
    0x5375, 0x6E53,  # "SunS" identifier at 40000 (mapped to 20-21 here)
    0x0001, 0x0042,  # Model ID 1, Length 66
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    # Address 30-99: Zero-initialized
] + [0] * 170  # Pad to 200 registers

# Input Registers (FC 04) - Address 0-199
# Pattern: address + 1000 (e.g., addr 0 = 1000, addr 1 = 1001)
INPUT_REGISTERS = [i + 1000 for i in range(200)]


class ModbusSimulator:
    """Modbus TCP server simulator for integration testing."""

    def __init__(self, port: int = 5020, unit_id: int = 1, verbose: bool = False):
        self.port = port
        self.unit_id = unit_id
        self.verbose = verbose
        self.server_thread: Optional[threading.Thread] = None
        self._running = False

        # Set up logging
        log_level = logging.DEBUG if verbose else logging.INFO
        logging.basicConfig(level=log_level)
        self.log = logging.getLogger(__name__)

    def create_datastore(self) -> ModbusServerContext:
        """Create and initialize the Modbus data store."""

        # Create data blocks for each register type
        # Note: pymodbus uses 1-based addressing internally, so we add 1 to the size
        # The first parameter is the starting address (0 for zero-based)

        coils = ModbusSequentialDataBlock(0, COILS_INITIAL)
        discrete_inputs = ModbusSequentialDataBlock(0, DISCRETE_INPUTS)
        holding_registers = ModbusSequentialDataBlock(0, HOLDING_REGISTERS_INITIAL.copy())
        input_registers = ModbusSequentialDataBlock(0, INPUT_REGISTERS)

        # Create slave context
        store = ModbusSlaveContext(
            di=discrete_inputs,    # FC 02
            co=coils,              # FC 01/05/15
            hr=holding_registers,  # FC 03/06/16/22/23
            ir=input_registers,    # FC 04
        )

        # Create server context with single slave
        context = ModbusServerContext(slaves={self.unit_id: store}, single=False)

        return context

    def create_identity(self) -> ModbusDeviceIdentification:
        """Create device identification for FC 17 (Report Server ID)."""
        identity = ModbusDeviceIdentification()
        identity.VendorName = "AdaModbus Test"
        identity.ProductCode = "TEST-001"
        identity.VendorUrl = "https://github.com/your-repo/adamodbus"
        identity.ProductName = "Modbus Simulator"
        identity.ModelName = "Integration Test Server"
        identity.MajorMinorRevision = "1.0.0"
        return identity

    def run(self):
        """Start the Modbus TCP server (blocking)."""
        self.log.info(f"Starting Modbus TCP Simulator on port {self.port}")
        self.log.info(f"Unit ID: {self.unit_id}")
        self.log.info(f"pymodbus version: {pymodbus_version}")
        self.log.info("")
        self.log.info("Test data configuration:")
        self.log.info(f"  Coils (FC 01): Addresses 0-99, alternating ON/OFF")
        self.log.info(f"  Discrete Inputs (FC 02): Addresses 0-99, every 3rd bit ON")
        self.log.info(f"  Holding Registers (FC 03): Addresses 0-199")
        self.log.info(f"    - Address 0-9: Sequential (0, 100, 200, ...)")
        self.log.info(f"    - Address 10-19: Test values (0x1234, 0x5678, ...)")
        self.log.info(f"  Input Registers (FC 04): Addresses 0-199 (value = address + 1000)")
        self.log.info("")
        self.log.info("Press Ctrl+C to stop the server")
        self.log.info("-" * 50)

        context = self.create_datastore()
        identity = self.create_identity()

        self._running = True

        try:
            StartTcpServer(
                context=context,
                identity=identity,
                address=("0.0.0.0", self.port),
            )
        except KeyboardInterrupt:
            self.log.info("Server stopped by user")
        except Exception as e:
            self.log.error(f"Server error: {e}")
            raise


def print_test_values():
    """Print expected test values for verification."""
    print("\nExpected Test Values for Verification:")
    print("=" * 60)

    print("\n1. Coils (FC 01 Read Coils / FC 05 Write Single Coil / FC 15 Write Multiple):")
    print("   Address 0-7: ON, OFF, ON, OFF, ON, OFF, ON, OFF")
    print("   (Pattern: even addresses are ON)")

    print("\n2. Discrete Inputs (FC 02 Read Discrete Inputs):")
    print("   Address 0-8: ON, OFF, OFF, ON, OFF, OFF, ON, OFF, OFF")
    print("   (Pattern: addresses divisible by 3 are ON)")

    print("\n3. Holding Registers (FC 03/06/16/22/23):")
    print("   Address 0:  0x0000 (0)")
    print("   Address 1:  0x0064 (100)")
    print("   Address 10: 0x1234 (4660)")
    print("   Address 11: 0x5678 (22136)")
    print("   Address 14: 0xAAAA (43690)")
    print("   Address 15: 0x5555 (21845)")

    print("\n4. Input Registers (FC 04 Read Input Registers):")
    print("   Address 0:  1000")
    print("   Address 1:  1001")
    print("   Address 99: 1099")

    print("\n5. Special Function Codes:")
    print("   FC 07 (Read Exception Status): Returns 0x00")
    print("   FC 08 (Diagnostics): Echo test supported")
    print("   FC 22 (Mask Write Register): Modifies holding registers")
    print("   FC 23 (Read/Write Multiple): Combined operation")

    print("\n" + "=" * 60)


def main():
    parser = argparse.ArgumentParser(
        description="Modbus TCP Simulator for AdaModbus Integration Tests"
    )
    parser.add_argument(
        "--port", "-p",
        type=int,
        default=5020,
        help="TCP port to listen on (default: 5020)"
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
    parser.add_argument(
        "--print-values",
        action="store_true",
        help="Print expected test values and exit"
    )

    args = parser.parse_args()

    if args.print_values:
        print_test_values()
        sys.exit(0)

    simulator = ModbusSimulator(
        port=args.port,
        unit_id=args.unit_id,
        verbose=args.verbose
    )

    try:
        simulator.run()
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
