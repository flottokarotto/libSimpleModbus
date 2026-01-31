# Cortex-M4 with LwIP

Modbus TCP examples for ARM Cortex-M microcontrollers using the LwIP TCP/IP stack.

## Quick Start (QEMU, no network)

The loopback test runs without any network setup:

```bash
# Install the ARM cross-compiler
alr toolchain --select gnat_arm_elf

# Build and run in QEMU
cd examples/embedded/cortex_m4_lwip
alr build
qemu-system-arm -M lm3s6965evb -nographic -semihosting -kernel bin/main_loopback.elf
```

Expected output:
```
=== Modbus Loopback Test ===
Test 1: Read Holding Registers (FC03)... PASS
Test 2: Write Single Register (FC06)... PASS
...
All tests PASSED!
```

Exit QEMU with `Ctrl+A`, then `X`.

## Available Programs

| Program | Description | Requires Network |
|---------|-------------|------------------|
| `main_loopback` | Self-test that verifies protocol encoding/decoding | No |
| `main_slave` | Modbus TCP server using LwIP | Yes |
| `main_master` | Modbus TCP client using LwIP | Yes |

## Building with LwIP

LwIP is included as a git submodule:

```bash
# Initialize the submodule
git submodule update --init

# Build with LwIP enabled
alr exec -- gprbuild -P cortex_m4_lwip.gpr -XLWIP=enabled
```

## Memory Usage

| Configuration | Flash | RAM |
|---------------|-------|-----|
| Loopback test only | ~11 KB | ~9 KB |
| With LwIP networking | ~45 KB | ~20 KB |

## Stack Size

The default 2 KB stack of the light runtime is too small for Modbus. The project increases it to 8 KB:

```gpr
package Linker is
   for Default_Switches ("Ada") use ("-Wl,--defsym=__stack_size=8192");
end Linker;
```

Symptom of stack overflow: QEMU shows "Lockup: can't escalate to HardFault" or the program crashes silently.

## Running with Real Network (Linux)

To test with actual TCP/IP networking in QEMU:

```bash
# Create a TAP interface (requires root)
sudo ip tuntap add dev tap0 mode tap user $USER
sudo ip link set tap0 up
sudo ip addr add 192.168.1.1/24 dev tap0

# Run QEMU with networking
qemu-system-arm -M netduino2 -nographic -semihosting \
  -netdev tap,id=net0,ifname=tap0,script=no,downscript=no \
  -device virtio-net-device,netdev=net0 \
  -kernel bin/main_slave.elf

# Connect from the host
modbus-cli read 192.168.1.10 0 10
```
