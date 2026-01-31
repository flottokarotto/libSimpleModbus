# Testing

## Running Unit Tests

The library uses [AUnit](https://github.com/AdaCore/aunit) for unit testing.

```bash
# Build the test runner
alr exec -- gprbuild -P tests/aunit_tests.gpr

# Run all tests
./bin/test_runner
```

## Unit Test Coverage

83 unit tests covering:

- **Checksums**: CRC-16 calculation and verification (RTU), LRC calculation (ASCII)
- **PDU encoding/decoding**: All function codes, exception responses, edge cases
- **Framing**: RTU frame building/parsing with CRC, ASCII frame building/parsing with LRC, TCP/MBAP header handling
- **Master**: Request building, response parsing, timeout handling, exception handling
- **Slave**: Request processing, callback invocation, exception generation
- **Async API**: Non-blocking request/response handling

## Integration Tests

Integration tests verify the library against a real Modbus TCP server (Python/pymodbus simulator).

```bash
# Install the simulator
pip install -r tests/integration/requirements.txt

# Start the simulator in one terminal
python tests/integration/modbus_simulator.py --port 5020

# Run the integration tests in another terminal
alr exec -- gprbuild -P tests/integration/integration_tests.gpr
./bin/integration_test_runner
```

### What is Tested

- All standard function codes (FC 01-06, 15, 16, 22, 23)
- Exception responses for invalid addresses
- Maximum register reads (125 registers)
- Write and read-back verification

## Code Coverage

The protocol core (encoding, decoding, framing) has 88-100% statement coverage. Master and Slave modules have lower coverage because some paths require specific hardware or network error conditions that are difficult to simulate.

## SPARK Verification

The protocol core is formally verified using SPARK. Run the proofs with:

```bash
alr with gnatprove
alr exec -- gnatprove -P adamodbus.gpr --mode=prove --level=1
```

All 1000+ verification conditions are proven, guaranteeing no buffer overflows, integer overflows, or uninitialized reads in the protocol core.
