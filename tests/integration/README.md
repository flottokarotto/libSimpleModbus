# Integration Tests

Integration tests verify AdaModbus against a real Modbus TCP server using a Python-based simulator.

## Prerequisites

Python 3.8+ with pymodbus:

```bash
pip install -r requirements.txt
```

## Running the Tests

Start the simulator and tests in separate terminals:

```bash
# Terminal 1: Start the Modbus TCP simulator
python modbus_simulator.py --port 5020

# Terminal 2: Build and run the Ada tests
alr exec -- gprbuild -P integration_tests.gpr
./bin/integration_test_runner
```

## Test Coverage

The integration tests verify all standard Modbus function codes:

| Function Code | Description | Test |
|---------------|-------------|------|
| FC 01 | Read Coils | Read 8 coils, verify bit pattern |
| FC 02 | Read Discrete Inputs | Read 9 inputs, verify pattern |
| FC 03 | Read Holding Registers | Read sequential and specific test values |
| FC 04 | Read Input Registers | Read and verify computed values |
| FC 05 | Write Single Coil | Write, read back, verify, restore |
| FC 06 | Write Single Register | Write, read back, verify, restore |
| FC 15 | Write Multiple Coils | Write 8 coils, verify pattern |
| FC 16 | Write Multiple Registers | Write 5 registers, verify |
| FC 22 | Mask Write Register | Apply AND/OR mask, verify result |
| FC 23 | Read/Write Multiple | Combined read and write operation |

Additional tests:
- Maximum register read (125 registers in one request)
- Invalid address handling (verify exception response)
- Wrong slave ID behavior (verify no response)

## Simulator Test Data

The simulator initializes with predictable data for verification:

- **Coils**: Even addresses ON, odd addresses OFF
- **Discrete Inputs**: Addresses divisible by 3 are ON
- **Holding Registers**: 0-9 contain sequential values (0, 100, 200...), 10-19 contain test patterns
- **Input Registers**: Value equals address + 1000

## Configuration

Edit `integration_tests.adb` to change connection settings:

```ada
Simulator_Host : constant String := "localhost";
Simulator_Port : constant Natural := 5020;
Test_Slave_Id  : constant Unit_Id := 1;
Test_Timeout   : constant Natural := 3000;  -- milliseconds
```
