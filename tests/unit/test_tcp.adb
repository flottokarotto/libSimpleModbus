--  Test_TCP - TCP/MBAP framing unit tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Protocol; use Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.TCP; use Ada_Modbus.Protocol.TCP;

package body Test_TCP is

   type TCP_Test_Case is new Test_Case with null record;

   overriding function Name (T : TCP_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("TCP/MBAP Framing Tests"));

   overriding procedure Register_Tests (T : in Out TCP_Test_Case);

   --  Test: Build TCP frame
   procedure Test_Build_Frame (T : in Out Test_Case'Class);
   procedure Test_Build_Frame (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      PDU     : PDU_Buffer := [others => 0];
      ADU     : ADU_Buffer;
      ADU_Len : Natural;
   begin
      --  FC03: 03 00 00 00 0A
      PDU (0) := 16#03#;
      PDU (1) := 16#00#;
      PDU (2) := 16#00#;
      PDU (3) := 16#00#;
      PDU (4) := 16#0A#;

      Build_Frame (ADU, ADU_Len, Transaction => 1234, Unit => 1,
                   PDU => PDU, PDU_Length => 5);

      --  MBAP: TransID(2) + ProtoID(2) + Len(2) + Unit(1) + PDU(5) = 12
      Assert (ADU_Len = 12, "ADU length should be 12");

      --  Transaction ID = 1234 = 0x04D2 (big endian)
      Assert (ADU (0) = 16#04#, "Trans ID high byte");
      Assert (ADU (1) = 16#D2#, "Trans ID low byte");

      --  Protocol ID = 0x0000
      Assert (ADU (2) = 16#00# and ADU (3) = 16#00#, "Protocol ID should be 0");

      --  Length = 6 (Unit ID + 5 PDU bytes)
      Assert (ADU (4) = 16#00# and ADU (5) = 16#06#, "Length should be 6");

      --  Unit ID
      Assert (ADU (6) = 16#01#, "Unit ID should be 1");

      --  PDU
      Assert (ADU (7) = 16#03#, "PDU FC should be 0x03");
   end Test_Build_Frame;

   --  Test: Parse valid TCP frame
   procedure Test_Parse_Valid_Frame (T : in Out Test_Case'Class);
   procedure Test_Parse_Valid_Frame (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      ADU         : ADU_Buffer := [others => 0];
      PDU         : PDU_Buffer;
      Transaction : Transaction_Id;
      Unit        : Unit_Id;
      PDU_Len     : Natural;
      Result      : Status;
   begin
      --  Build a valid MBAP frame manually
      ADU (0) := 16#00#;  --  Trans ID high
      ADU (1) := 16#01#;  --  Trans ID low = 1
      ADU (2) := 16#00#;  --  Proto ID high
      ADU (3) := 16#00#;  --  Proto ID low
      ADU (4) := 16#00#;  --  Length high
      ADU (5) := 16#06#;  --  Length low = 6
      ADU (6) := 16#01#;  --  Unit ID
      ADU (7) := 16#03#;  --  FC
      ADU (8) := 16#00#;
      ADU (9) := 16#00#;
      ADU (10) := 16#00#;
      ADU (11) := 16#0A#;

      Parse_Frame (ADU, 12, Transaction, Unit, PDU, PDU_Len, Result);

      Assert (Result = Success, "Parse should succeed");
      Assert (Transaction = 1, "Transaction ID should be 1");
      Assert (Unit = 1, "Unit ID should be 1");
      Assert (PDU_Len = 5, "PDU length should be 5");
      Assert (PDU (0) = 16#03#, "FC should be 0x03");
   end Test_Parse_Valid_Frame;

   --  Test: Invalid protocol ID
   procedure Test_Invalid_Protocol_ID (T : in Out Test_Case'Class);
   procedure Test_Invalid_Protocol_ID (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      ADU         : ADU_Buffer := [others => 0];
      PDU         : PDU_Buffer;
      Transaction : Transaction_Id;
      Unit        : Unit_Id;
      PDU_Len     : Natural;
      Result      : Status;
   begin
      ADU (0) := 16#00#;
      ADU (1) := 16#01#;
      ADU (2) := 16#00#;
      ADU (3) := 16#01#;  --  Wrong protocol ID
      ADU (4) := 16#00#;
      ADU (5) := 16#02#;
      ADU (6) := 16#01#;
      ADU (7) := 16#03#;

      Parse_Frame (ADU, 8, Transaction, Unit, PDU, PDU_Len, Result);

      Assert (Result = Frame_Error, "Should fail with invalid protocol ID");
   end Test_Invalid_Protocol_ID;

   --  Test: Get_Transaction_Id
   procedure Test_Get_Transaction_Id (T : in Out Test_Case'Class);
   procedure Test_Get_Transaction_Id (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      ADU : ADU_Buffer := [others => 0];
   begin
      ADU (0) := 16#12#;
      ADU (1) := 16#34#;
      Assert (Get_Transaction_Id (ADU) = 16#1234#, "Transaction ID should be 0x1234");

      ADU (0) := 16#00#;
      ADU (1) := 16#00#;
      Assert (Get_Transaction_Id (ADU) = 0, "Transaction ID should be 0");

      ADU (0) := 16#FF#;
      ADU (1) := 16#FF#;
      Assert (Get_Transaction_Id (ADU) = 16#FFFF#, "Transaction ID should be 0xFFFF");
   end Test_Get_Transaction_Id;

   --  Test: Get_Expected_Length
   --  Returns total frame length (6 MBAP bytes before length + length field value)
   procedure Test_Get_Expected_Length (T : in Out Test_Case'Class);
   procedure Test_Get_Expected_Length (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      ADU : ADU_Buffer := [others => 0];
   begin
      --  Length field = 6 -> total frame = 6 + 6 = 12
      ADU (4) := 16#00#;
      ADU (5) := 16#06#;
      Assert (Get_Expected_Length (ADU) = 12, "Expected length should be 12 (6 MBAP + 6)");

      --  Length field = 1 -> total frame = 6 + 1 = 7
      ADU (4) := 16#00#;
      ADU (5) := 16#01#;
      Assert (Get_Expected_Length (ADU) = 7, "Expected length should be 7 (6 MBAP + 1)");
   end Test_Get_Expected_Length;

   --  Test: Frame too short
   procedure Test_Frame_Too_Short (T : in Out Test_Case'Class);
   procedure Test_Frame_Too_Short (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      ADU         : constant ADU_Buffer := [others => 0];
      PDU         : PDU_Buffer;
      Transaction : Transaction_Id;
      Unit        : Unit_Id;
      PDU_Len     : Natural;
      Result      : Status;
   begin
      Parse_Frame (ADU, 7, Transaction, Unit, PDU, PDU_Len, Result);  --  < 8
      Assert (Result = Frame_Error, "Frame too short should fail");
   end Test_Frame_Too_Short;

   --  Test: Length field too small (< 2)
   procedure Test_Length_Field_Too_Small (T : in Out Test_Case'Class);
   procedure Test_Length_Field_Too_Small (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      ADU         : ADU_Buffer := [others => 0];
      PDU         : PDU_Buffer;
      Transaction : Transaction_Id;
      Unit        : Unit_Id;
      PDU_Len     : Natural;
      Result      : Status;
   begin
      ADU (0) := 16#00#;
      ADU (1) := 16#01#;
      ADU (2) := 16#00#;
      ADU (3) := 16#00#;  --  Valid protocol ID
      ADU (4) := 16#00#;
      ADU (5) := 16#01#;  --  Length = 1 (< 2, invalid)
      ADU (6) := 16#01#;
      ADU (7) := 16#03#;

      Parse_Frame (ADU, 8, Transaction, Unit, PDU, PDU_Len, Result);
      Assert (Result = Frame_Error, "Length field < 2 should fail");
   end Test_Length_Field_Too_Small;

   --  Test: Length field too large (> 254)
   procedure Test_Length_Field_Too_Large (T : in Out Test_Case'Class);
   procedure Test_Length_Field_Too_Large (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      ADU         : ADU_Buffer := [others => 0];
      PDU         : PDU_Buffer;
      Transaction : Transaction_Id;
      Unit        : Unit_Id;
      PDU_Len     : Natural;
      Result      : Status;
   begin
      ADU (0) := 16#00#;
      ADU (1) := 16#01#;
      ADU (2) := 16#00#;
      ADU (3) := 16#00#;
      ADU (4) := 16#00#;
      ADU (5) := 16#FF#;  --  Length = 255 (> 254, invalid)
      ADU (6) := 16#01#;
      ADU (7) := 16#03#;

      Parse_Frame (ADU, 8, Transaction, Unit, PDU, PDU_Len, Result);
      Assert (Result = Frame_Error, "Length field > 254 should fail");
   end Test_Length_Field_Too_Large;

   --  Test: Length field mismatch with actual ADU length
   procedure Test_Length_Mismatch (T : in Out Test_Case'Class);
   procedure Test_Length_Mismatch (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      ADU         : ADU_Buffer := [others => 0];
      PDU         : PDU_Buffer;
      Transaction : Transaction_Id;
      Unit        : Unit_Id;
      PDU_Len     : Natural;
      Result      : Status;
   begin
      ADU (0) := 16#00#;
      ADU (1) := 16#01#;
      ADU (2) := 16#00#;
      ADU (3) := 16#00#;
      ADU (4) := 16#00#;
      ADU (5) := 16#06#;  --  Length = 6 -> expect 12 bytes total
      ADU (6) := 16#01#;
      ADU (7) := 16#03#;

      Parse_Frame (ADU, 10, Transaction, Unit, PDU, PDU_Len, Result);  --  10 /= 12
      Assert (Result = Frame_Error, "Length mismatch should fail");
   end Test_Length_Mismatch;

   --  Test: Invalid Unit ID (> 247)
   procedure Test_Invalid_Unit_Id (T : in Out Test_Case'Class);
   procedure Test_Invalid_Unit_Id (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      ADU         : ADU_Buffer := [others => 0];
      PDU         : PDU_Buffer;
      Transaction : Transaction_Id;
      Unit        : Unit_Id;
      PDU_Len     : Natural;
      Result      : Status;
   begin
      ADU (0) := 16#00#;
      ADU (1) := 16#01#;
      ADU (2) := 16#00#;
      ADU (3) := 16#00#;
      ADU (4) := 16#00#;
      ADU (5) := 16#02#;  --  Length = 2
      ADU (6) := 16#F8#;  --  Unit ID = 248 > 247
      ADU (7) := 16#03#;

      Parse_Frame (ADU, 8, Transaction, Unit, PDU, PDU_Len, Result);
      Assert (Result = Frame_Error, "Unit ID > 247 should fail");
   end Test_Invalid_Unit_Id;

   --  Test: Round-trip build and parse
   procedure Test_Round_Trip (T : in Out Test_Case'Class);
   procedure Test_Round_Trip (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      PDU_In        : PDU_Buffer := [others => 0];
      PDU_Out       : PDU_Buffer;
      ADU           : ADU_Buffer;
      ADU_Len       : Natural;
      Trans_Out     : Transaction_Id;
      Unit_Out      : Unit_Id;
      PDU_Len_Out   : Natural;
      Result        : Status;
   begin
      PDU_In (0) := 16#06#;
      PDU_In (1) := 16#00#;
      PDU_In (2) := 16#10#;
      PDU_In (3) := 16#12#;
      PDU_In (4) := 16#34#;

      Build_Frame (ADU, ADU_Len, Transaction => 9999, Unit => 5,
                   PDU => PDU_In, PDU_Length => 5);
      Parse_Frame (ADU, ADU_Len, Trans_Out, Unit_Out, PDU_Out, PDU_Len_Out, Result);

      Assert (Result = Success, "Round-trip should succeed");
      Assert (Trans_Out = 9999, "Transaction ID should match");
      Assert (Unit_Out = 5, "Unit ID should match");
      Assert (PDU_Len_Out = 5, "PDU length should match");
      for I in 0 .. 4 loop
         Assert (PDU_Out (I) = PDU_In (I), "PDU byte" & I'Image & " should match");
      end loop;
   end Test_Round_Trip;

   overriding procedure Register_Tests (T : in Out TCP_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_Build_Frame'Access, "Build TCP frame");
      Registration.Register_Routine (T, Test_Parse_Valid_Frame'Access, "Parse valid frame");
      Registration.Register_Routine (T, Test_Invalid_Protocol_ID'Access, "Invalid protocol ID");
      Registration.Register_Routine (T, Test_Get_Transaction_Id'Access, "Get_Transaction_Id");
      Registration.Register_Routine (T, Test_Get_Expected_Length'Access, "Get_Expected_Length");
      Registration.Register_Routine (T, Test_Round_Trip'Access, "Round-trip");
      --  Error path tests
      Registration.Register_Routine (T, Test_Frame_Too_Short'Access, "Frame too short");
      Registration.Register_Routine (T, Test_Length_Field_Too_Small'Access, "Length field too small");
      Registration.Register_Routine (T, Test_Length_Field_Too_Large'Access, "Length field too large");
      Registration.Register_Routine (T, Test_Length_Mismatch'Access, "Length mismatch");
      Registration.Register_Routine (T, Test_Invalid_Unit_Id'Access, "Invalid Unit ID");
   end Register_Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new TCP_Test_Case);
      return S;
   end Suite;

end Test_TCP;
