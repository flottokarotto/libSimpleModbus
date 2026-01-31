# Minimal RTU Example

A minimal Modbus RTU slave example for embedded systems. This is the simplest possible implementation for resource-constrained microcontrollers.

## Features

- No TCP/IP stack required - just UART
- Compatible with ZFP and Light runtimes
- Minimal RAM footprint (~2 KB)
- Simple callback-based register handling

## Building

```bash
alr toolchain --select gnat_arm_elf
alr exec -- gprbuild -P minimal_rtu.gpr --target=arm-eabi --RTS=light-cortex-m4f
```

## Integration Steps

1. **Implement UART functions** for your hardware:
   ```ada
   function UART_Send (Data : Byte_Array) return Natural;
   function UART_Receive (Buffer : out Byte_Array; Timeout_Ms : Natural) return Natural;
   ```

2. **Implement register callbacks**:
   ```ada
   function Read_Holding_Registers
     (Start : Register_Address; Qty : Register_Count; Values : out Register_Array) return Status;
   ```

3. **Call `Modbus_Poll`** from your main loop to process incoming requests.

## Memory Usage

| Component | Size |
|-----------|------|
| Request buffer | 256 bytes |
| Response buffer | 256 bytes |
| Stack per call | ~1 KB |
| **Total** | ~2-3 KB |
