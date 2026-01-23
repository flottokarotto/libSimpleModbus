<p align="center">
  <img src="logo.png" alt="AdaModbus" width="600">
</p>


# AdaModbus

[![CI](https://github.com/flottokarotto/AdaModbus/actions/workflows/ci.yml/badge.svg)](https://github.com/flottokarotto/AdaModbus/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/flottokarotto/AdaModbus/graph/badge.svg)](https://codecov.io/gh/flottokarotto/AdaModbus)
[![SPARK](https://img.shields.io/badge/SPARK-99%25%20proven-brightgreen)](https://www.adacore.com/about-spark)

Ada 2022 Modbus library for embedded and desktop systems.

## Features

- **Protocols**: Modbus RTU, ASCII, and TCP
- **Roles**: Master (Client) and Slave (Server)
- **ZFP-compatible core**: No tasking, exceptions, or dynamic allocation in protocol layer
- **SPARK verified**: 99% of runtime checks formally proven (see below)
- **Cross-platform**: Works on Windows, Linux, and embedded systems
- **Transport abstraction**: Generic design allows custom transport backends

## Supported Function Codes

| Code | Function                     | Master | Slave |
| ---- | ---------------------------- | ------ | ----- |
| 01   | Read Coils                   | ✓     | ✓    |
| 02   | Read Discrete Inputs         | ✓     | ✓    |
| 03   | Read Holding Registers       | ✓     | ✓    |
| 04   | Read Input Registers         | ✓     | ✓    |
| 05   | Write Single Coil            | ✓     | ✓    |
| 06   | Write Single Register        | ✓     | ✓    |
| 07   | Read Exception Status        | ✓     | ✓    |
| 08   | Diagnostics                  | ✓     | ✓    |
| 15   | Write Multiple Coils         | ✓     | ✓    |
| 16   | Write Multiple Registers     | ✓     | ✓    |
| 17   | Report Server ID             | ✓     | ✓    |
| 22   | Mask Write Register          | ✓     | ✓    |
| 23   | Read/Write Multiple Registers| ✓     | ✓    |

## Building

Requires [Alire](https://alire.ada.dev/) (Ada package manager).

```bash
# Build library
alr build

# Build and run tests (AUnit as submodule)
alr exec -- gprbuild -P tests/aunit_tests.gpr -aP aunit/lib/gnat
./bin/test_runner

# Build examples
alr exec -- gprbuild -P examples/examples.gpr
```

### Without Alire

If you have GPRbuild and GNAT installed:

```bash
gprbuild -P adamodbus.gpr
```

## Testing

See [TESTING.md](TESTING.md) for test coverage documentation.

## Formal Verification (SPARK)

The protocol core is formally verified using [SPARK](https://www.adacore.com/about-spark), a subset of Ada designed for high-assurance software.

### Verification Results

| Metric | Value |
|--------|-------|
| Total checks | 421 |
| Proven | 416 (99%) |
| Unproven | 5 (1%) |

The 5 unproven checks are for unconstrained array bounds that depend on caller-provided data - these are protected by runtime checks.

### Verified Properties

- **No buffer overflows**: All array accesses proven within bounds
- **No integer overflows**: Arithmetic operations proven safe
- **No uninitialized variables**: All data paths verified
- **Correct data flow**: No unexpected aliasing or side effects

### Verified Packages

All protocol packages have `SPARK_Mode => On`:

- `Ada_Modbus` - Base types
- `Ada_Modbus.Protocol` - PDU encoding/decoding
- `Ada_Modbus.Protocol.RTU/ASCII/TCP` - Framing layers
- `Ada_Modbus.CRC16`, `Ada_Modbus.LRC` - Checksums
- `Ada_Modbus.Utilities` - Byte order conversion

### Running SPARK Analysis

```bash
# Add gnatprove dependency
alr with gnatprove

# Run proof analysis
alr exec -- gnatprove -P adamodbus.gpr --mode=prove --level=1
```

## Usage

### Master (Client) Example

```ada
with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Master;
with Ada_Modbus.Transport.TCP; use Ada_Modbus.Transport.TCP;

procedure Example is
   Connection : aliased TCP_Connection;

   --  Transport callbacks with context
   function Send (Ctx : in out TCP_Connection; Data : Byte_Array) return Natural is
   begin
      return Ada_Modbus.Transport.TCP.Send (Ctx, Data);
   end Send;

   function Receive
     (Ctx : in out TCP_Connection; Buffer : out Byte_Array;
      Max : Natural; Timeout : Natural) return Natural is
   begin
      return Ada_Modbus.Transport.TCP.Receive (Ctx, Buffer, Max, Timeout);
   end Receive;

   function Get_Tick return Unsigned_32 is (0);  --  Simplified for example

   --  Instantiate master with TCP_Connection as transport context
   package My_Master is new Ada_Modbus.Master
     (Transport_Context => TCP_Connection,
      Send              => Send,
      Receive           => Receive,
      Get_Tick_Ms       => Get_Tick);

   Ctx    : My_Master.Master_Context;
   Values : Register_Array (0 .. 9);
   Result : Status;
begin
   --  Connect to Modbus TCP server
   Connect (Connection, "192.168.1.100", 502, 5.0, Result);
   if Result /= Success then
      return;
   end if;

   --  Initialize master with connection
   My_Master.Initialize
     (Ctx,
      (Mode => My_Master.TCP, Default_Slave => 1, Default_Timeout => 1000),
      Connection);

   --  Read 10 holding registers from address 0
   Result := My_Master.Read_Holding_Registers
     (Ctx, Slave => 1, Start_Address => 0, Quantity => 10, Values => Values);

   if Result = Success then
      --  Process values...
      null;
   end if;

   Disconnect (Connection);
end Example;
```

### Slave (Server) Example

```ada
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Slave; use Ada_Modbus.Slave;

procedure Example is
   Registers : Register_Array (0 .. 99) := [others => 0];

   function Read_Holding_Registers
     (Start : Register_Address; Qty : Register_Count; Values : out Register_Array) return Status
   is
   begin
      for I in 0 .. Natural (Qty) - 1 loop
         Values (Values'First + I) := Registers (Natural (Start) + I);
      end loop;
      return Success;
   end Read_Holding_Registers;

   Config : constant Slave_Config :=
     (Mode      => TCP,
      Unit_Id   => 1,
      Callbacks => (Read_Holding_Registers => Read_Holding_Registers'Unrestricted_Access,
                    others => null));

   Request  : Byte_Array (0 .. 259);
   Response : Byte_Array (0 .. 259);
   Req_Len, Resp_Len : Natural;
   Send_Resp : Boolean;
begin
   --  When request is received:
   Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send_Resp);
   --  Send Response (0 .. Resp_Len - 1) if Send_Resp is True
end Example;
```

### Async Master (Non-blocking)

For event-loop or polling-based applications:

```ada
with Ada_Modbus.Master.Async;

--  Instantiate async extension
package My_Async is new My_Master.Async (Max_Pending_Requests => 4);

--  Callback for register reads
procedure On_Response
  (Handle      : My_Async.Request_Handle;
   Resp_Status : My_Async.Response_Status;
   Slave       : Unit_Id;
   Values      : Register_Array;
   Exc_Code    : Byte)
is
begin
   if Resp_Status = My_Async.Response_Success then
      --  Process received values
      null;
   end if;
end On_Response;

Async_Ctx : My_Async.Async_Context;
Handle    : My_Async.Request_Handle;
Success   : Boolean;
begin
   My_Async.Initialize (Async_Ctx, Master_Ctx);

   --  Start async request (returns immediately)
   Success := My_Async.Read_Holding_Registers_Async
     (Async_Ctx, Slave => 1, Start_Address => 0, Quantity => 10,
      On_Response => On_Response'Access, Handle => Handle);

   --  Main loop
   loop
      My_Async.Process_Pending (Async_Ctx);  --  Polls for responses
      --  ... other work ...
   end loop;
end;
```

## Demo Programs

### Ada

```bash
./bin/tcp_slave [port]     # Server (default: 1502)
./bin/tcp_master [host] [port]  # Client (default: localhost 1502)
```

### C

```bash
./bin/c_tcp_slave [port]
./bin/c_tcp_master [host] [port]
```

Build C examples:

```bash
cd examples/c && alr exec -- make
```

## C API

For C developers who want to use the library without writing Ada code.

### Build

```bash
# Desktop
alr build

# Cross-compile for ARM Cortex-M
alr toolchain --select gnat_arm_elf
alr build -- --target=arm-eabi --RTS=light-cortex-m4f -XADAMODBUS_BUILD_MODE=release
```

### Files

Copy these to your project:

| File                          | Description                  |
| ----------------------------- | ---------------------------- |
| `lib/adamodbus.a`     | Static library               |
| `src/c_api/ada_modbus.h` | C header (API documentation) |

With **Light runtime**: No additional dependencies.
With **Light-Tasking/Full**: Also link `-lgnat`.

### Link

```bash
# Desktop (Windows)
gcc myapp.c -Llib -ladamodbus -lgnat -lws2_32 -o myapp.exe

# Desktop (Linux)
gcc myapp.c -Llib -ladamodbus -lgnat -lpthread -o myapp

# Embedded (Light runtime)
arm-none-eabi-gcc myapp.c -Llib -ladamodbus -o firmware.elf
```

### API

See `src/c_api/ada_modbus.h` for full API documentation.
See `examples/c/c_tcp_master.c` and `examples/c/c_tcp_slave.c` for usage examples.

## Package Structure

```
Ada_Modbus                         -- Base types (Byte, Register, Status, etc.)
├── Ada_Modbus.Utilities           -- Byte order conversion
├── Ada_Modbus.CRC16               -- CRC-16 for RTU
├── Ada_Modbus.LRC                 -- LRC for ASCII
├── Ada_Modbus.Protocol            -- PDU encoding/decoding
│   ├── Ada_Modbus.Protocol.RTU    -- RTU framing
│   ├── Ada_Modbus.Protocol.ASCII  -- ASCII framing
│   └── Ada_Modbus.Protocol.TCP    -- MBAP/TCP framing
├── Ada_Modbus.Master              -- Master (Client) - generic
│   └── Ada_Modbus.Master.Async    -- Non-blocking async API
├── Ada_Modbus.Slave               -- Slave (Server) - callback-based
└── Ada_Modbus.Transport           -- Transport abstraction
    ├── Ada_Modbus.Transport.TCP   -- TCP socket backend
    └── Ada_Modbus.Transport.Serial -- Serial port (COM/TTY) backend
```

## Directory Structure

```
AdaModbus/
├── src/
│   ├── core/           -- Protocol core (ZFP-compatible)
│   └── transport/      -- Transport backends
├── tests/
│   └── unit/           -- AUnit tests
├── examples/           -- Demo programs
├── adamodbus.gpr -- Main GPR project
├── alire.toml          -- Alire manifest
└── README.md
```

## License

[MIT](LICENSE)

## Disclaimer

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
