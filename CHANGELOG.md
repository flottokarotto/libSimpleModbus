# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2026-01-25

### Added

#### Core Protocol
- Modbus RTU, ASCII, and TCP protocol support
- Master (Client) with synchronous and asynchronous APIs
- Slave (Server) with callback-based request handling
- All standard function codes (FC 01-06, 07, 08, 15, 16, 17, 22, 23)
- 100% SPARK-verified protocol core

#### Transport Backends
- TCP socket transport (Windows/Linux)
- Serial port transport (COM/TTY) for RTU/ASCII
- TLS transport (separate `adamodbus_tls` crate)

#### Energy Management
- **SunSpec Alliance profiles**:
  - Model 1: Common (device information)
  - Models 101-103: Inverters (single/split/three-phase)
  - Model 120: Nameplate ratings
  - Model 121: Basic settings
  - Model 124: Basic storage
  - Model 160: Multiple MPPT
  - Models 201-204: Meters (1P/SP/3P Wye/Delta)
  - Models 701/704: DER AC/Control
  - Model 802: Battery (extended)
- **go-e Charger**: `Ada_Modbus.Energy.Go_E` package for go-e wallbox
- SG-Ready heat pump control
- §14a EnWG grid power limitation

#### C API
- Base TCP master/slave API (`ada_modbus.h`)
- SunSpec high-level API (`ada_modbus_sunspec.h`)
- Serial/RTU API (`ada_modbus_serial.h`)

#### Examples
- TCP master/slave demos
- RTU master/slave demos
- ASCII master/slave demos
- Async master demo
- Kostal inverter reader + dashboard
- KSEM energy meter reader + dashboard
- go-e charger dashboard + simulator
- C language examples
- Embedded examples (Cortex-M4 with LwIP, STM32 RTU)

#### Build & Test
- Alire package manager support
- GitHub Actions CI (build, test, SPARK, embedded)
- Automated release workflow with binary artifacts
- 114 unit tests with AUnit
- Integration tests with Python Modbus simulator
- Code coverage with Codecov

### Technical Details

- Ada 2022 standard
- ZFP-compatible core (no tasking, exceptions, or dynamic allocation)
- Generic transport abstraction for custom backends
- 32-bit word order support (ABCD, CDAB, BADC, DCBA)
- Signed scale factor support for SunSpec

[1.0.0]: https://github.com/flottokarotto/AdaModbus/releases/tag/v1.0.0
