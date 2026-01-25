--  Test_RTU - RTU framing unit tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Protocol; use Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.RTU; use Ada_Modbus.Protocol.RTU;

package body Test_RTU is

   type RTU_Test_Case is new Test_Case with null record;

   overriding function Name (T : RTU_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("RTU Framing Tests"));

   overriding procedure Register_Tests (T : in Out RTU_Test_Case);

   --  Test: Build RTU frame
   procedure Test_Build_Frame (T : in Out Test_Case'Class);
   procedure Test_Build_Frame (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      PDU       : PDU_Buffer := [others => 0];
      ADU       : ADU_Buffer;
      ADU_Len   : Natural;
   begin
      --  FC03 Read Holding Registers: 03 00 00 00 0A
      PDU (0) := 16#03#;
      PDU (1) := 16#00#;
      PDU (2) := 16#00#;
      PDU (3) := 16#00#;
      PDU (4) := 16#0A#;

      Build_Frame (ADU, ADU_Len, Slave => 1, PDU => PDU, PDU_Length => 5);

      Assert (ADU_Len = 8, "ADU length should be 8 (1 addr + 5 PDU + 2 CRC)");
      Assert (ADU (0) = 16#01#, "First byte should be slave address");
      Assert (ADU (1) = 16#03#, "Second byte should be function code");
      --  CRC should be valid
      Assert (Verify_CRC (ADU (0 .. ADU_Len - 1)), "CRC should be valid");
   end Test_Build_Frame;

   --  Test: Parse valid RTU frame
   procedure Test_Parse_Valid_Frame (T : in Out Test_Case'Class);
   procedure Test_Parse_Valid_Frame (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      --  Valid frame: Slave=01, FC=03, Data=00 00 00 0A, CRC=C5 CD
      ADU     : ADU_Buffer := [others => 0];
      PDU     : PDU_Buffer;
      Slave   : Unit_Id;
      PDU_Len : Natural;
      Result  : Status;
   begin
      ADU (0) := 16#01#;  --  Slave
      ADU (1) := 16#03#;  --  FC
      ADU (2) := 16#00#;  --  Start High
      ADU (3) := 16#00#;  --  Start Low
      ADU (4) := 16#00#;  --  Qty High
      ADU (5) := 16#0A#;  --  Qty Low
      ADU (6) := 16#C5#;  --  CRC Low
      ADU (7) := 16#CD#;  --  CRC High

      Parse_Frame (ADU, 8, Slave, PDU, PDU_Len, Result);

      Assert (Result = Success, "Parse should succeed");
      Assert (Slave = 1, "Slave should be 1");
      Assert (PDU_Len = 5, "PDU length should be 5");
      Assert (PDU (0) = 16#03#, "FC should be 0x03");
   end Test_Parse_Valid_Frame;

   --  Test: Parse frame with invalid CRC
   procedure Test_Parse_Invalid_CRC (T : in Out Test_Case'Class);
   procedure Test_Parse_Invalid_CRC (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      ADU     : ADU_Buffer := [others => 0];
      PDU     : PDU_Buffer;
      Slave   : Unit_Id;
      PDU_Len : Natural;
      Result  : Status;
   begin
      ADU (0) := 16#01#;
      ADU (1) := 16#03#;
      ADU (2) := 16#00#;
      ADU (3) := 16#00#;
      ADU (4) := 16#00#;
      ADU (5) := 16#0A#;
      ADU (6) := 16#00#;  --  Wrong CRC
      ADU (7) := 16#00#;

      Parse_Frame (ADU, 8, Slave, PDU, PDU_Len, Result);

      Assert (Result = CRC_Error, "Parse should fail with CRC_Error");
   end Test_Parse_Invalid_CRC;

   --  Test: Frame too short
   procedure Test_Frame_Too_Short (T : in Out Test_Case'Class);
   procedure Test_Frame_Too_Short (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      ADU     : constant ADU_Buffer := [others => 0];
      PDU     : PDU_Buffer;
      Slave   : Unit_Id;
      PDU_Len : Natural;
      Result  : Status;
   begin
      --  Minimum frame is 4 bytes (addr + FC + 2 CRC)
      Parse_Frame (ADU, 3, Slave, PDU, PDU_Len, Result);
      Assert (Result = Frame_Error, "Should fail for frame < 4 bytes");
   end Test_Frame_Too_Short;

   --  Test: Invalid Unit ID (> 247)
   procedure Test_Invalid_Unit_Id (T : in Out Test_Case'Class);
   procedure Test_Invalid_Unit_Id (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      ADU     : ADU_Buffer := [others => 0];
      PDU     : PDU_Buffer;
      Slave   : Unit_Id;
      PDU_Len : Natural;
      Result  : Status;
   begin
      --  Unit ID 248 > 247, need valid CRC
      --  Frame: F8 03 + CRC (0x7102)
      ADU (0) := 16#F8#;  --  248 > 247
      ADU (1) := 16#03#;  --  FC
      ADU (2) := 16#02#;  --  CRC Low
      ADU (3) := 16#71#;  --  CRC High

      Parse_Frame (ADU, 4, Slave, PDU, PDU_Len, Result);
      Assert (Result = Frame_Error, "Unit ID > 247 should fail");
   end Test_Invalid_Unit_Id;

   --  Test: Round-trip build and parse
   procedure Test_Round_Trip (T : in Out Test_Case'Class);
   procedure Test_Round_Trip (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      PDU_In    : PDU_Buffer := [others => 0];
      PDU_Out   : PDU_Buffer;
      ADU       : ADU_Buffer;
      ADU_Len   : Natural;
      Slave_Out : Unit_Id;
      PDU_Len_Out : Natural;
      Result    : Status;
   begin
      --  Create test PDU
      PDU_In (0) := 16#06#;  --  FC Write Single Register
      PDU_In (1) := 16#00#;
      PDU_In (2) := 16#10#;  --  Address 16
      PDU_In (3) := 16#12#;
      PDU_In (4) := 16#34#;  --  Value 0x1234

      Build_Frame (ADU, ADU_Len, Slave => 5, PDU => PDU_In, PDU_Length => 5);
      Parse_Frame (ADU, ADU_Len, Slave_Out, PDU_Out, PDU_Len_Out, Result);

      Assert (Result = Success, "Round-trip should succeed");
      Assert (Slave_Out = 5, "Slave should match");
      Assert (PDU_Len_Out = 5, "PDU length should match");
      for I in 0 .. 4 loop
         Assert (PDU_Out (I) = PDU_In (I), "PDU byte" & I'Image & " should match");
      end loop;
   end Test_Round_Trip;

   overriding procedure Register_Tests (T : in Out RTU_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_Build_Frame'Access, "Build RTU frame");
      Registration.Register_Routine (T, Test_Parse_Valid_Frame'Access, "Parse valid frame");
      Registration.Register_Routine (T, Test_Parse_Invalid_CRC'Access, "Parse invalid CRC");
      Registration.Register_Routine (T, Test_Frame_Too_Short'Access, "Frame too short");
      Registration.Register_Routine (T, Test_Invalid_Unit_Id'Access, "Invalid Unit ID");
      Registration.Register_Routine (T, Test_Round_Trip'Access, "Round-trip");
   end Register_Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new RTU_Test_Case);
      return S;
   end Suite;

end Test_RTU;
