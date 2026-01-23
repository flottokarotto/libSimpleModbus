--  Ada_Modbus.CRC16 - CRC-16 calculation for Modbus RTU
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Modbus RTU uses CRC-16 with polynomial 0x8005 (reflected: 0xA001)
--  Initial value: 0xFFFF
--  The result is appended LSB first (Little-Endian) to the frame

with Interfaces;

package Ada_Modbus.CRC16
  with SPARK_Mode => On
is

   pragma Pure;

   subtype CRC_Value is Interfaces.Unsigned_16;

   --  Calculate CRC-16 for a byte array
   function Calculate (Data : Byte_Array) return CRC_Value;

   --  Verify CRC-16 (data includes CRC bytes at the end)
   --  Returns True if CRC is valid
   function Verify (Data : Byte_Array) return Boolean
     with Pre => Data'Length >= 2;

   --  Extract CRC from frame (last 2 bytes, LSB first)
   function Extract (Data : Byte_Array) return CRC_Value
     with Pre => Data'Length >= 2;

   --  Append CRC to buffer, returns new length
   procedure Append
     (Buffer : in out Byte_Array;
      Length : in out Natural;
      CRC    : CRC_Value)
     with Pre => Buffer'Length >= 2
                 and then Length <= Buffer'Length - 2
                 and then Buffer'First + Length + 1 <= Buffer'Last
                 and then Length <= Natural'Last - 2;
   pragma Annotate (GNATprove, False_Positive,
                    "range check might fail",
                    "Buffer'Length >= 2 ensures subtraction is valid");

end Ada_Modbus.CRC16;
