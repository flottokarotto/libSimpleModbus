--  Test_SunSpec - SunSpec energy package tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Protocol; use Ada_Modbus.Protocol;
with Ada_Modbus.Energy.SunSpec; use Ada_Modbus.Energy.SunSpec;

package body Test_SunSpec is

   type SunSpec_Test_Case is new Test_Case with null record;

   overriding function Name (T : SunSpec_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("SunSpec Energy Package Tests"));

   overriding procedure Register_Tests (T : in out SunSpec_Test_Case);

   --  Test: Scale factor application
   procedure Test_Apply_Scale (T : in out Test_Case'Class);
   procedure Test_Apply_Scale (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      --  Scale factor 0: no scaling
      Assert (Apply_Scale (100, 0) = 100.0, "SF=0 should not scale");

      --  Scale factor 1: multiply by 10
      Assert (Apply_Scale (100, 1) = 1000.0, "SF=1 should multiply by 10");

      --  Scale factor -1: divide by 10
      Assert (Apply_Scale (100, -1) = 10.0, "SF=-1 should divide by 10");

      --  Scale factor 2: multiply by 100
      Assert (Apply_Scale (50, 2) = 5000.0, "SF=2 should multiply by 100");

      --  Scale factor -2: divide by 100
      Assert (Apply_Scale (1000, -2) = 10.0, "SF=-2 should divide by 100");
   end Test_Apply_Scale;

   --  Test: SunSpec identifier check request encoding
   procedure Test_Check_SunSpec_Request (T : in Out Test_Case'Class);
   procedure Test_Check_SunSpec_Request (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      Encode_Check_SunSpec_Request
        (Base_Address => 40000,
         Buffer       => Buffer,
         Length       => Length);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#03#, "FC should be Read Holding Registers (0x03)");
      --  Address 40000 = 0x9C40
      Assert (Buffer (1) = 16#9C#, "Address high byte should be 0x9C");
      Assert (Buffer (2) = 16#40#, "Address low byte should be 0x40");
      --  Quantity = 2
      Assert (Buffer (3) = 16#00#, "Quantity high should be 0x00");
      Assert (Buffer (4) = 16#02#, "Quantity low should be 0x02");
   end Test_Check_SunSpec_Request;

   --  Test: SunSpec identifier response decoding (valid)
   procedure Test_Check_SunSpec_Response_Valid (T : in Out Test_Case'Class);
   procedure Test_Check_SunSpec_Response_Valid (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer   : PDU_Buffer := [others => 0];
      Is_Valid : Boolean;
      Result   : Status;
   begin
      --  Build valid response: FC=03, ByteCount=4, "SunS" (0x53756E53)
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 16#04#;  --  Byte count
      Buffer (2) := 16#53#;  --  'S'
      Buffer (3) := 16#75#;  --  'u'
      Buffer (4) := 16#6E#;  --  'n'
      Buffer (5) := 16#53#;  --  'S'

      Decode_Check_SunSpec_Response
        (Buffer   => Buffer,
         Length   => 6,
         Is_Valid => Is_Valid,
         Result   => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (Is_Valid, "Should detect valid SunSpec device");
   end Test_Check_SunSpec_Response_Valid;

   --  Test: SunSpec identifier response decoding (invalid)
   procedure Test_Check_SunSpec_Response_Invalid (T : in Out Test_Case'Class);
   procedure Test_Check_SunSpec_Response_Invalid (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer   : PDU_Buffer := [others => 0];
      Is_Valid : Boolean;
      Result   : Status;
   begin
      --  Build invalid response: wrong identifier
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 16#04#;  --  Byte count
      Buffer (2) := 16#00#;  --  Wrong
      Buffer (3) := 16#00#;
      Buffer (4) := 16#00#;
      Buffer (5) := 16#00#;

      Decode_Check_SunSpec_Response
        (Buffer   => Buffer,
         Length   => 6,
         Is_Valid => Is_Valid,
         Result   => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (not Is_Valid, "Should detect non-SunSpec device");
   end Test_Check_SunSpec_Response_Invalid;

   --  Test: Model header request encoding
   procedure Test_Read_Model_Header_Request (T : in Out Test_Case'Class);
   procedure Test_Read_Model_Header_Request (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      --  Read header at offset 2 (after SunS identifier)
      Encode_Read_Model_Header_Request
        (Base_Address => 40000,
         Offset       => 2,
         Buffer       => Buffer,
         Length       => Length);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#03#, "FC should be Read Holding Registers");
      --  Address 40002 = 0x9C42
      Assert (Buffer (1) = 16#9C#, "Address high byte should be 0x9C");
      Assert (Buffer (2) = 16#42#, "Address low byte should be 0x42");
   end Test_Read_Model_Header_Request;

   --  Test: Model header response decoding
   procedure Test_Decode_Model_Header (T : in Out Test_Case'Class);
   procedure Test_Decode_Model_Header (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Header : Model_Header;
      Result : Status;
   begin
      --  Build response: Model ID = 1 (Common), Length = 66
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 16#04#;  --  Byte count
      Buffer (2) := 16#00#;  --  Model ID high
      Buffer (3) := 16#01#;  --  Model ID low (1 = Common)
      Buffer (4) := 16#00#;  --  Length high
      Buffer (5) := 16#42#;  --  Length low (66)

      Decode_Model_Header_Response
        (Buffer => Buffer,
         Length => 6,
         Header => Header,
         Result => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (Header.ID = 1, "Model ID should be 1 (Common)");
      Assert (Header.Length = 66, "Model length should be 66");
   end Test_Decode_Model_Header;

   --  Test: String decoding from registers
   procedure Test_Decode_String (T : in Out Test_Case'Class);
   procedure Test_Decode_String (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Registers : Register_Array (0 .. 3);
      Result    : SunSpec_String;
      Len       : Natural;
   begin
      --  "SMA" encoded as 16-bit registers (big-endian)
      Registers (0) := 16#534D#;  --  "SM"
      Registers (1) := 16#4100#;  --  "A" + NUL
      Registers (2) := 16#0000#;  --  NUL terminator
      Registers (3) := 16#0000#;

      Decode_String (Registers, Result, Len);

      Assert (Len = 3, "String length should be 3");
      Assert (Result (1 .. 3) = "SMA", "String should be 'SMA'");
   end Test_Decode_String;

   --  Test: Scale factor lookup table bounds
   procedure Test_Scale_Multipliers (T : in Out Test_Case'Class);
   procedure Test_Scale_Multipliers (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      --  Test extreme scale factors
      Assert (Scale_Multipliers (-10) = 1.0E-10, "SF=-10 multiplier");
      Assert (Scale_Multipliers (10) = 1.0E10, "SF=10 multiplier");
      Assert (Scale_Multipliers (0) = 1.0, "SF=0 multiplier");
   end Test_Scale_Multipliers;

   ---------------------
   -- Register_Tests --
   ---------------------

   overriding procedure Register_Tests (T : in out SunSpec_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_Apply_Scale'Access, "Apply_Scale");
      Registration.Register_Routine (T, Test_Check_SunSpec_Request'Access,
                          "Encode_Check_SunSpec_Request");
      Registration.Register_Routine (T, Test_Check_SunSpec_Response_Valid'Access,
                          "Decode_Check_SunSpec_Response (valid)");
      Registration.Register_Routine (T, Test_Check_SunSpec_Response_Invalid'Access,
                          "Decode_Check_SunSpec_Response (invalid)");
      Registration.Register_Routine (T, Test_Read_Model_Header_Request'Access,
                          "Encode_Read_Model_Header_Request");
      Registration.Register_Routine (T, Test_Decode_Model_Header'Access,
                          "Decode_Model_Header_Response");
      Registration.Register_Routine (T, Test_Decode_String'Access,
                          "Decode_String");
      Registration.Register_Routine (T, Test_Scale_Multipliers'Access,
                          "Scale_Multipliers lookup table");
   end Register_Tests;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite :=
        new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new SunSpec_Test_Case);
      return S;
   end Suite;

end Test_SunSpec;
