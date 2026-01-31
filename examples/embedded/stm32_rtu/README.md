# STM32 Modbus RTU

Modbus RTU master and slave examples for STM32, testable with QEMU.

The examples use the LM3S6965EVB board emulation in QEMU because it has good semihosting support for debug output.

## Building

```bash
cd examples/embedded/stm32_rtu

# Build the slave
alr exec -- gprbuild -P stm32_rtu.gpr -XMAIN=slave

# Build the master
alr exec -- gprbuild -P stm32_rtu.gpr -XMAIN=master
```

## Running in QEMU

### Single Instance

```bash
# Linux/MSYS2
./run_qemu.sh slave
./run_qemu.sh master

# Windows CMD
run_qemu.bat slave
run_qemu.bat master
```

### Master-Slave Communication

The UART is connected via a TCP socket (port 5555), allowing two QEMU instances to communicate:

```bash
# Terminal 1: Start the slave (listens on socket)
./run_qemu.sh slave-server

# Terminal 2: Start the master (connects to socket)
./run_qemu.sh master-client
```

Exit QEMU with `Ctrl+A`, then `X`.

### CI Test

Run both instances automatically and verify communication:

```bash
./run_ci_test.sh
```

## Slave Register Map

| Address | Type | Description |
|---------|------|-------------|
| 0-31 | Holding Registers | Read/write storage |
| 0-31 | Input Registers | Read-only values |
| 0 | Input Register | Request counter |
| 1 | Input Register | Uptime in seconds |
| 0-63 | Coils | Read/write bits |

## Master Test Sequence

The master polls the slave every 500ms:

1. Read holding registers 0-4
2. Write an incrementing counter to register 0
3. Read coils 0-7
4. Toggle a coil
5. Read input registers 0-1 (request count and uptime)

## Porting to Real Hardware

The UART driver (`lm3s_uart.ads`) uses LM3S6965-specific register addresses. For real STM32 hardware, you'll need to replace it with a driver that uses STM32 peripheral addresses.
