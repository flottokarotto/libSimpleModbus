# AdaModbus Integration Tests

This directory contains integration tests that test the AdaModbus library against a real Modbus TCP server (pymodbus-based simulator).

## Overview

Unlike unit tests that use mocks, these integration tests verify the complete communication path:

```
AdaModbus (Master) <--TCP/IP--> pymodbus Simulator (Slave)
```

## Prerequisites

### Python and pymodbus

The simulator requires Python 3.8+ and pymodbus:

```bash
# Install pymodbus
pip install pymodbus

# Verify installation
python -c "from pymodbus.server import StartTcpServer; print('pymodbus installed')"
```

### Ada/GNAT

The test runner requires:
- GNAT FSF or GNAT Community (with Ada 2022 support)
- AUnit (via Alire: `alr with aunit`)

## Directory Contents

| File | Description |
|------|-------------|
| `modbus_simulator.py` | Python-based Modbus TCP server with predefined test data |
| `integration_tests.ads/adb` | Ada integration test suite |
| `integration_test_runner.adb` | Ada test runner main program |
| `integration_tests.gpr` | GPRbuild project file |
| `README.md` | This file |

## Running the Tests

### Step 1: Start the Modbus Simulator

```bash
# From the tests/integration directory
python modbus_simulator.py --port 5020

# Or with verbose logging
python modbus_simulator.py --port 5020 --verbose
```

The simulator will start listening on TCP port 5020 (default) with Unit ID 1.

### Step 2: Build and Run the Ada Tests

In a separate terminal:

```bash
# From the project root
cd /path/to/libSimpleModbus

# Build the integration tests
alr build -- -P tests/integration/integration_tests.gpr

# Run the tests
./bin/integration_test_runner
```

### Expected Output

```
========================================
  AdaModbus Integration Test Suite
========================================

This test suite requires the Modbus simulator to be running.
Start it with:
  python modbus_simulator.py --port 5020

Starting tests...
----------------------------------------

Modbus TCP Integration Tests
   FC01 Read Coils : PASSED
   FC02 Read Discrete Inputs : PASSED
   FC03 Read Holding Registers : PASSED
   ...
   Total Tests Run:  14
   Successful Tests: 14
   Failed Tests:     0

========================================
  Tests completed
========================================
```

## Test Coverage

The integration tests cover the following Modbus function codes:

| Function Code | Name | Test |
|--------------|------|------|
| FC 01 | Read Coils | Read 8 coils, verify pattern |
| FC 02 | Read Discrete Inputs | Read 9 inputs, verify pattern |
| FC 03 | Read Holding Registers | Read sequential and test values |
| FC 04 | Read Input Registers | Read and verify computed values |
| FC 05 | Write Single Coil | Write, read-back, restore |
| FC 06 | Write Single Register | Write, read-back, restore |
| FC 15 | Write Multiple Coils | Write 8 coils, verify |
| FC 16 | Write Multiple Registers | Write 5 registers, verify |
| FC 22 | Mask Write Register | Apply AND/OR mask, verify |
| FC 23 | Read/Write Multiple Registers | Combined operation |

Additional tests:
- Maximum register read (125 registers)
- Invalid address handling (exception response)
- Wrong slave ID behavior

## Simulator Test Data

The simulator initializes with specific test data for verification:

### Coils (FC 01/05/15) - Addresses 0-99
- Pattern: Even addresses are ON, odd addresses are OFF
- Example: Coil 0=ON, 1=OFF, 2=ON, 3=OFF, ...

### Discrete Inputs (FC 02) - Addresses 0-99
- Pattern: Addresses divisible by 3 are ON
- Example: Input 0=ON, 1=OFF, 2=OFF, 3=ON, 4=OFF, 5=OFF, 6=ON, ...

### Holding Registers (FC 03/06/16/22/23) - Addresses 0-199
- Addresses 0-9: Sequential (0, 100, 200, 300, ...)
- Addresses 10-19: Test values:
  - 10: 0x1234
  - 11: 0x5678
  - 12: 0x9ABC
  - 13: 0xDEF0
  - 14: 0xAAAA
  - 15: 0x5555
  - 16: 0xFF00
  - 17: 0x00FF
  - 18: 0x8000
  - 19: 0x7FFF

### Input Registers (FC 04) - Addresses 0-199
- Pattern: Value = Address + 1000
- Example: Register 0=1000, 1=1001, 99=1099

## Configuration

### Changing Port or Host

Edit `integration_tests.adb` and modify:

```ada
Simulator_Host : constant String := "localhost";
Simulator_Port : constant Natural := 5020;
Test_Slave_Id  : constant Unit_Id := 1;
Test_Timeout   : constant Natural := 3000;
```

### Changing Simulator Data

Edit `modbus_simulator.py` and modify the initial data arrays:
- `COILS_INITIAL`
- `DISCRETE_INPUTS`
- `HOLDING_REGISTERS_INITIAL`
- `INPUT_REGISTERS`

## Troubleshooting

### "Could not connect to simulator"

1. Ensure the Python simulator is running
2. Check the port number matches (default: 5020)
3. Check no firewall is blocking the connection
4. Try `telnet localhost 5020` to verify connectivity

### Simulator Crashes

Check the pymodbus version:
```bash
pip show pymodbus
```

Requires pymodbus >= 3.0.0 for the current API.

### Tests Timeout

Increase the timeout value in `integration_tests.adb`:
```ada
Test_Timeout : constant Natural := 5000;  -- 5 seconds
```

## CI/CD Integration

See the section below for GitHub Actions integration.

## GitHub Actions Integration

To run these integration tests in CI, add the following to `.github/workflows/ci.yml`:

```yaml
name: CI

on: [push, pull_request]

jobs:
  unit-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install Alire
        uses: alire-project/setup-alire@v3

      - name: Build and run unit tests
        run: |
          alr build -- -P tests/aunit_tests.gpr
          ./bin/test_runner

  integration-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install Alire
        uses: alire-project/setup-alire@v3

      - name: Set up Python
        uses: actions/setup-python@v5
        with:
          python-version: '3.11'

      - name: Install pymodbus
        run: pip install pymodbus

      - name: Start Modbus Simulator
        run: |
          python tests/integration/modbus_simulator.py --port 5020 &
          sleep 2  # Wait for server to start

      - name: Build integration tests
        run: alr build -- -P tests/integration/integration_tests.gpr

      - name: Run integration tests
        run: ./bin/integration_test_runner

      - name: Stop Modbus Simulator
        if: always()
        run: pkill -f modbus_simulator.py || true
```

### Windows CI

For Windows runners, use:

```yaml
  integration-tests-windows:
    runs-on: windows-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install Alire
        uses: alire-project/setup-alire@v3

      - name: Set up Python
        uses: actions/setup-python@v5
        with:
          python-version: '3.11'

      - name: Install pymodbus
        run: pip install pymodbus

      - name: Start Modbus Simulator
        shell: pwsh
        run: |
          Start-Process python -ArgumentList "tests/integration/modbus_simulator.py","--port","5020" -NoNewWindow
          Start-Sleep -Seconds 2

      - name: Build integration tests
        run: alr build -- -P tests/integration/integration_tests.gpr

      - name: Run integration tests
        run: ./bin/integration_test_runner.exe
```

## License

MIT License - see LICENSE file in the project root.
