--  Ada_Modbus.Transport.TLS_Mbed.Hardware_RNG - Hardware RNG Integration
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides integration with hardware random number generators for
--  embedded systems. This is CRITICAL for TLS security!
--
--  Without a true hardware RNG, the default software entropy gathering
--  may be predictable, compromising TLS security.
--
--  Usage:
--    1. Implement the RNG_Callback for your platform
--    2. Call Register_Hardware_RNG before Connect/Listen
--    3. The hardware RNG will be used as entropy source
--
--  Example for STM32:
--    function STM32_RNG_Read
--      (Output : System.Address;
--       Len    : Natural;
--       Olen   : out Natural) return Boolean
--    is
--       type Byte_Ptr is access all Byte;
--       Buf : Byte_Ptr := To_Pointer (Output);
--    begin
--       for I in 0 .. Len - 1 loop
--          --  Wait for RNG ready and read from RNG_DR register
--          while (RNG.SR and RNG_SR_DRDY) = 0 loop null; end loop;
--          Buf.all := Byte (RNG.DR and 16#FF#);
--          Buf := To_Pointer (To_Address (Buf) + 1);
--       end loop;
--       Olen := Len;
--       return True;
--    end STM32_RNG_Read;

with System;

package Ada_Modbus.Transport.TLS_Mbed.Hardware_RNG is

   pragma Preelaborate;

   --  Hardware RNG callback type
   --  Output: Address of buffer to fill with random bytes
   --  Len: Number of bytes requested
   --  Olen: Out parameter - actual bytes written
   --  Returns: True on success, False on failure
   type RNG_Callback is access function
     (Output : System.Address;
      Len    : Natural;
      Olen   : out Natural) return Boolean;

   --  Register a hardware RNG as entropy source
   --  This should be called BEFORE Connect or Listen
   --  The registered RNG will be used for all subsequent TLS operations
   --
   --  Callback: Your platform-specific RNG function
   --  Returns: True if successfully registered
   function Register_Hardware_RNG (Callback : RNG_Callback) return Boolean
     with Post => (if Register_Hardware_RNG'Result then Is_Hardware_RNG_Registered);

   --  Check if a hardware RNG is registered
   function Is_Hardware_RNG_Registered return Boolean;

   --  Unregister the hardware RNG (reverts to software entropy)
   procedure Unregister_Hardware_RNG
     with Post => not Is_Hardware_RNG_Registered;

   --  Get the registered callback (for internal use)
   function Get_Registered_Callback return RNG_Callback;

end Ada_Modbus.Transport.TLS_Mbed.Hardware_RNG;
