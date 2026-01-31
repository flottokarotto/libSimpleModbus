<p align="center">
  <img src="logo.svg" alt="AdaModbus" width="450">
</p>

# AdaModbus

[![CI](https://github.com/flottokarotto/AdaModbus/actions/workflows/ci.yml/badge.svg)](https://github.com/flottokarotto/AdaModbus/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/flottokarotto/AdaModbus/graph/badge.svg)](https://codecov.io/gh/flottokarotto/AdaModbus)
[![SPARK](https://img.shields.io/badge/SPARK-verified-blue)](https://www.adacore.com/about-spark)

Ada 2022 Modbus library for embedded and desktop systems.

## Features

- **Protocols**: Modbus RTU, ASCII, and TCP
- **Roles**: Master (client) and Slave (server) with all standard function codes
- **SPARK verified**: The protocol core is formally proven - no buffer overflows, no integer overflows
- **ZFP-compatible**: The core uses no tasking, exceptions, or dynamic allocation, so it runs on bare-metal microcontrollers
- **Energy management**: Built-in support for SunSpec inverter/meter profiles, SG-Ready heat pump control, and §14a grid power limitation
- **C API**: Use the library from C/C++ without writing Ada code

## Status

**Tested with real hardware:**
- Modbus TCP communication with Kostal PLENTICORE inverter
- Modbus TCP communication with Kostal Smart Energy Meter (KSEM)
- SunSpec model discovery and register reading on these devices

**Unit tests only (no real hardware testing yet):**
- Modbus RTU and ASCII framing
- Serial port transport on Windows

**Experimental (compiles and runs in simulation):**
- TLS transport - not tested with actual Modbus/TCP Security devices
- Embedded ARM builds - runs in QEMU, but not deployed on real hardware yet

Contributions and hardware testing feedback are welcome!

## Quick Start

Requires [Alire](https://alire.ada.dev/) (Ada package manager).

```bash
# Build the library
alr build

# Build and run unit tests
alr exec -- gprbuild -P tests/aunit_tests.gpr
./bin/test_runner

# Build example programs
alr exec -- gprbuild -P examples/examples.gpr
```

## Usage Example

```ada
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Transport.TCP; use Ada_Modbus.Transport.TCP;

procedure Example is
   Connection : TCP_Connection;
   Values     : Register_Array (0 .. 9);
   Result     : Status;
begin
   --  Connect to a Modbus TCP server
   Connect (Connection, "192.168.1.100", 502, 5.0, Result);
   if Result /= Success then
      return;
   end if;

   --  Read 10 holding registers starting at address 0
   Result := My_Master.Read_Holding_Registers
     (Ctx, Slave => 1, Start_Address => 0, Quantity => 10, Values => Values);

   if Result = Success then
      --  Process the values...
   end if;

   Disconnect (Connection);
end Example;
```

## Supported Function Codes

| Code | Function | Master | Slave |
|------|----------|--------|-------|
| 01 | Read Coils | Yes | Yes |
| 02 | Read Discrete Inputs | Yes | Yes |
| 03 | Read Holding Registers | Yes | Yes |
| 04 | Read Input Registers | Yes | Yes |
| 05 | Write Single Coil | Yes | Yes |
| 06 | Write Single Register | Yes | Yes |
| 15 | Write Multiple Coils | Yes | Yes |
| 16 | Write Multiple Registers | Yes | Yes |
| 22 | Mask Write Register | Yes | Yes |
| 23 | Read/Write Multiple Registers | Yes | Yes |

## Packages

| Package | Description |
|---------|-------------|
| `Ada_Modbus.Protocol` | PDU encoding/decoding (SPARK verified) |
| `Ada_Modbus.Protocol.RTU/ASCII/TCP` | Framing layers (SPARK verified) |
| `Ada_Modbus.Master` | Client with synchronous and asynchronous API |
| `Ada_Modbus.Slave` | Server with callback-based request handling |
| `Ada_Modbus.Transport.TCP` | TCP transport using GNAT.Sockets |
| `Ada_Modbus.Transport.Serial` | Serial port transport for RTU/ASCII |
| `Ada_Modbus.Energy.SunSpec` | SunSpec profiles for inverters, meters, batteries |
| `Ada_Modbus.Energy.SG_Ready` | Heat pump control via SG-Ready protocol |
| `Ada_Modbus.Energy.Grid_Control` | §14a EnWG grid power limitation |
| `Ada_Modbus.C_API` | C bindings for use without Ada |

## TLS Support

TLS is provided as separate crates to avoid pulling in dependencies when not needed.

| Crate | Backend | Platform | Use Case |
|-------|---------|----------|----------|
| `adamodbus_tls` | AWS / OpenSSL | Desktop (Windows, Linux) | Servers and workstations |
| `adamodbus_tls_mbed` | mbedTLS | Embedded (Cortex-M) | Microcontrollers |

## Example Programs

```bash
# Basic TCP communication
./bin/tcp_master localhost 1502      # Connect to a Modbus TCP server
./bin/tcp_slave 1502                 # Start a Modbus TCP server

# Energy monitoring dashboards
./bin/kostal_dashboard 192.168.1.10  # Live dashboard for Kostal inverters
./bin/ksem_dashboard 192.168.1.11    # Live dashboard for Kostal energy meters
./bin/go_e_dashboard 192.168.1.50    # Live dashboard for go-e EV chargers

# RTU over serial port
./bin/rtu_master COM3                # RTU master on serial port
./bin/rtu_slave COM3                 # RTU slave on serial port
```

## Documentation

- [TESTING.md](TESTING.md) - Test coverage and how to run tests
- [CHANGELOG.md](CHANGELOG.md) - Release notes

## License

[MIT](LICENSE)
