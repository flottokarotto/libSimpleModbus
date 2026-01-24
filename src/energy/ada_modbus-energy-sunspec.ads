--  Ada_Modbus.Energy.SunSpec - SunSpec Modbus Profile
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  SunSpec Alliance defines standardized Modbus registers for:
--  - Solar inverters (Models 101-103)
--  - Battery storage (Models 124, 802)
--  - Smart meters (Models 201-204)
--  - DER controls (Models 701-714)
--
--  Reference: https://sunspec.org/sunspec-modbus-specifications/
--
--  SunSpec devices use a "SunS" identifier at the base address,
--  followed by concatenated model blocks. Each model has a header:
--    - Register 0: Model ID (uint16)
--    - Register 1: Model Length (uint16, excluding header)

with Ada_Modbus.Protocol;

package Ada_Modbus.Energy.SunSpec
  with SPARK_Mode => On
is

   --  Standard SunSpec base addresses (holding registers)
   Default_Base_Address : constant Register_Address := 40_000;
   Alt_Base_Address     : constant Register_Address := 50_000;

   --  SunSpec "SunS" identifier (0x53756E53)
   SunS_ID_High : constant Register_Value := 16#5375#;  --  "Su"
   SunS_ID_Low  : constant Register_Value := 16#6E53#;  --  "nS"

   --  End of model marker
   End_Model_ID : constant Register_Value := 16#FFFF#;

   --  Common SunSpec Model IDs
   Model_Common         : constant := 1;
   Model_Inverter_1P    : constant := 101;  --  Single phase
   Model_Inverter_SP    : constant := 102;  --  Split phase
   Model_Inverter_3P    : constant := 103;  --  Three phase
   Model_Nameplate      : constant := 120;
   Model_Basic_Settings : constant := 121;
   Model_Measurements   : constant := 122;
   Model_Immed_Controls : constant := 123;  --  Immediate controls
   Model_Storage        : constant := 124;  --  Basic storage
   Model_MPPT           : constant := 160;  --  Multiple MPPT
   Model_Meter_1P       : constant := 201;
   Model_Meter_SP       : constant := 202;
   Model_Meter_3P_WYE   : constant := 203;
   Model_Meter_3P_Delta : constant := 204;

   --  SunSpec data types
   type Model_ID is new Interfaces.Unsigned_16;
   type Model_Length is new Interfaces.Unsigned_16;

   --  Scale factors (sunssf type: -10 to +10)
   type Scale_Factor is range -10 .. 10;

   --  Lookup table for scale factors (avoids floating-point exponentiation)
   Scale_Multipliers : constant array (Scale_Factor) of Float :=
     [-10 => 1.0E-10,
      -9  => 1.0E-9,
      -8  => 1.0E-8,
      -7  => 1.0E-7,
      -6  => 1.0E-6,
      -5  => 1.0E-5,
      -4  => 1.0E-4,
      -3  => 1.0E-3,
      -2  => 1.0E-2,
      -1  => 1.0E-1,
       0  => 1.0,
       1  => 1.0E1,
       2  => 1.0E2,
       3  => 1.0E3,
       4  => 1.0E4,
       5  => 1.0E5,
       6  => 1.0E6,
       7  => 1.0E7,
       8  => 1.0E8,
       9  => 1.0E9,
       10 => 1.0E10];

   --  Apply scale factor to a register value (unsigned)
   function Apply_Scale (Value : Register_Value; SF : Scale_Factor) return Float
     with Inline;

   --  Apply scale factor to a signed register value (e.g., temperature)
   --  SunSpec int16 values are stored as unsigned but represent signed data
   function Apply_Scale_Signed
     (Value : Register_Value; SF : Scale_Factor) return Float
     with Inline;

   --  SunSpec string (max 16 registers = 32 chars)
   subtype SunSpec_String is String (1 .. 32);

   --  Decode SunSpec string from registers (padded with NUL or spaces)
   procedure Decode_String
     (Registers : Register_Array;
      Result    : out SunSpec_String;
      Length    : out Natural)
     with Pre => Registers'Length <= 16;

   --  Model header info
   type Model_Header is record
      ID     : Model_ID;
      Length : Model_Length;
   end record;

   --  Check for SunSpec identifier at given address
   --  Encodes a request to read the 2-register SunS identifier
   procedure Encode_Check_SunSpec_Request
     (Base_Address : Register_Address;
      Buffer       : out Protocol.PDU_Buffer;
      Length       : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Decode SunSpec identifier check response
   procedure Decode_Check_SunSpec_Response
     (Buffer   : Protocol.PDU_Buffer;
      Length   : Natural;
      Is_Valid : out Boolean;
      Result   : out Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

   --  Read model header at specified offset from base
   procedure Encode_Read_Model_Header_Request
     (Base_Address : Register_Address;
      Offset       : Register_Address;
      Buffer       : out Protocol.PDU_Buffer;
      Length       : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Decode model header response
   procedure Decode_Model_Header_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Header : out Model_Header;
      Result : out Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

end Ada_Modbus.Energy.SunSpec;
