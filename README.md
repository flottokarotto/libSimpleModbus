<p align="center">
  <img src="logo.svg" alt="AdaModbus" width="450">
</p>

# AdaModbus

[![CI](https://github.com/flottokarotto/AdaModbus/actions/workflows/ci.yml/badge.svg)](https://github.com/flottokarotto/AdaModbus/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/flottokarotto/AdaModbus/graph/badge.svg)](https://codecov.io/gh/flottokarotto/AdaModbus)
[![SPARK](https://img.shields.io/badge/SPARK-verified-blue)](https://www.adacore.com/about-spark)

Ada 2022 Modbus library for embedded and desktop systems.

## Features

- **Protocols**: RTU, ASCII, TCP
- **Roles**: Master and Slave
- **SPARK verified**: Protocol core formally proven
- **ZFP-compatible**: No tasking, exceptions, or dynamic allocation
- **Energy**: SunSpec, SG-Ready, §14a grid control

## Status

**Getestet** mit echter Hardware:
- Modbus TCP mit Kostal PLENTICORE Wechselrichter
- Modbus TCP mit Kostal Smart Energy Meter (KSEM)
- SunSpec Inverter/Meter Models

**Nur Unit Tests:**
- Modbus RTU, ASCII
- Serial Transport (Windows)

**Experimentell:**
- TLS (kompiliert, nicht mit echten Geräten getestet)
- Embedded ARM (läuft in QEMU, keine echte Hardware)

## Quick Start

```bash
alr build
alr exec -- gprbuild -P tests/aunit_tests.gpr
./bin/test_runner
```

## Usage

```ada
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Transport.TCP; use Ada_Modbus.Transport.TCP;

--  Connect
Connect (Connection, "192.168.1.100", 502, 5.0, Result);

--  Read 10 holding registers
Result := My_Master.Read_Holding_Registers
  (Ctx, Slave => 1, Start_Address => 0, Quantity => 10, Values => Values);

Disconnect (Connection);
```

## Packages

| Package | Description |
|---------|-------------|
| `Ada_Modbus.Protocol` | PDU encoding/decoding (SPARK) |
| `Ada_Modbus.Protocol.RTU/ASCII/TCP` | Framing layers (SPARK) |
| `Ada_Modbus.Master` | Client with sync/async API |
| `Ada_Modbus.Slave` | Server with callbacks |
| `Ada_Modbus.Transport.TCP` | GNAT.Sockets backend |
| `Ada_Modbus.Energy.SunSpec` | Solar inverters, meters, batteries |
| `Ada_Modbus.C_API` | C bindings |

## TLS Support

| Crate | Backend | Platform |
|-------|---------|----------|
| `adamodbus_tls` | AWS/OpenSSL | Desktop |
| `adamodbus_tls_mbed` | mbedTLS | Embedded |

## Examples

```bash
./bin/tcp_master localhost 1502      # TCP client
./bin/tcp_slave 1502                 # TCP server
./bin/kostal_dashboard 192.168.1.10  # SunSpec inverter
./bin/go_e_dashboard 192.168.1.50    # EV charger
```

## Documentation

- [TESTING.md](TESTING.md) - Test coverage
- [CHANGELOG.md](CHANGELOG.md) - Release notes
- [CLAUDE.md](CLAUDE.md) - Development guide

## License

[MIT](LICENSE)
