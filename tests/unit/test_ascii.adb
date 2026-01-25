--  Test_ASCII - ASCII framing unit tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Protocol; use Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.ASCII; use Ada_Modbus.Protocol.ASCII;

package body Test_ASCII is

   type ASCII_Test_Case is new Test_Case with null record;

   overriding function Name (T : ASCII_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("ASCII Framing Tests"));

   overriding procedure Register_Tests (T : in Out ASCII_Test_Case);

   --  Test: Byte to Hex conversion
   procedure Test_Byte_To_Hex (T : in Out Test_Case'Class);
   procedure Test_Byte_To_Hex (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      High, Low : Byte;
   begin
      Byte_To_Hex (16#00#, High, Low);
      Assert (High = Character'Pos ('0') and Low = Character'Pos ('0'), "0x00 -> '00'");

      Byte_To_Hex (16#0A#, High, Low);
      Assert (High = Character'Pos ('0') and Low = Character'Pos ('A'), "0x0A -> '0A'");

      Byte_To_Hex (16#FF#, High, Low);
      Assert (High = Character'Pos ('F') and Low = Character'Pos ('F'), "0xFF -> 'FF'");

      Byte_To_Hex (16#1B#, High, Low);
      Assert (High = Character'Pos ('1') and Low = Character'Pos ('B'), "0x1B -> '1B'");
   end Test_Byte_To_Hex;

   --  Test: Hex to Byte conversion
   procedure Test_Hex_To_Byte (T : in Out Test_Case'Class);
   procedure Test_Hex_To_Byte (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Result : Byte;
   begin
      Result := Hex_To_Byte (Character'Pos ('0'), Character'Pos ('0'));
      Assert (Result = 16#00#, "'00' -> 0x00");

      Result := Hex_To_Byte (Character'Pos ('0'), Character'Pos ('A'));
      Assert (Result = 16#0A#, "'0A' -> 0x0A");

      Result := Hex_To_Byte (Character'Pos ('F'), Character'Pos ('F'));
      Assert (Result = 16#FF#, "'FF' -> 0xFF");

      --  Test lowercase
      Result := Hex_To_Byte (Character'Pos ('a'), Character'Pos ('b'));
      Assert (Result = 16#AB#, "'ab' -> 0xAB");
   end Test_Hex_To_Byte;

   --  Test: Is_Hex_Digit
   procedure Test_Is_Hex_Digit (T : in Out Test_Case'Class);
   procedure Test_Is_Hex_Digit (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (Is_Hex_Digit (Character'Pos ('0')), "'0' is hex");
      Assert (Is_Hex_Digit (Character'Pos ('9')), "'9' is hex");
      Assert (Is_Hex_Digit (Character'Pos ('A')), "'A' is hex");
      Assert (Is_Hex_Digit (Character'Pos ('F')), "'F' is hex");
      Assert (Is_Hex_Digit (Character'Pos ('a')), "'a' is hex");
      Assert (Is_Hex_Digit (Character'Pos ('f')), "'f' is hex");
      Assert (not Is_Hex_Digit (Character'Pos ('G')), "'G' is not hex");
      Assert (not Is_Hex_Digit (Character'Pos ('/')), "'/' is not hex");
      Assert (not Is_Hex_Digit (Character'Pos (':')), "':' is not hex");
   end Test_Is_Hex_Digit;

   --  Test: Build ASCII frame
   procedure Test_Build_Frame (T : in Out Test_Case'Class);
   procedure Test_Build_Frame (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      PDU       : PDU_Buffer := [others => 0];
      Frame     : Frame_Buffer;
      Frame_Len : Natural;
   begin
      --  FC03: 03 00 00 00 0A
      PDU (0) := 16#03#;
      PDU (1) := 16#00#;
      PDU (2) := 16#00#;
      PDU (3) := 16#00#;
      PDU (4) := 16#0A#;

      Build_Frame (Frame, Frame_Len, Slave => 1, PDU => PDU, PDU_Length => 5);

      --  Frame should be: :0103000000xxCRLF
      --  : + 01 + 03 + 00 + 00 + 00 + 0A + LRC(2) + CRLF = 1 + 12 + 2 + 2 = 17
      Assert (Frame_Len = 17, "Frame length should be 17, got" & Frame_Len'Image);
      Assert (Frame (0) = Frame_Start, "Should start with ':'");
      Assert (Frame (Frame_Len - 2) = Frame_CR, "Should end with CR");
      Assert (Frame (Frame_Len - 1) = Frame_LF, "Should end with LF");
   end Test_Build_Frame;

   --  Test: Parse valid ASCII frame
   procedure Test_Parse_Valid_Frame (T : in Out Test_Case'Class);
   procedure Test_Parse_Valid_Frame (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      --  :0103000000AAF2CRLF
      --  Slave=01, FC=03, Data=00 00 00 0A, LRC=F2
      Frame     : Frame_Buffer := [others => 0];
      PDU       : PDU_Buffer;
      Slave     : Unit_Id;
      PDU_Len   : Natural;
      Result    : Status;
      Idx       : Natural := 0;
   begin
      Frame (Idx) := Frame_Start; Idx := Idx + 1;  --  :
      Frame (Idx) := Character'Pos ('0'); Idx := Idx + 1;  --  0
      Frame (Idx) := Character'Pos ('1'); Idx := Idx + 1;  --  1 (slave)
      Frame (Idx) := Character'Pos ('0'); Idx := Idx + 1;  --  0
      Frame (Idx) := Character'Pos ('3'); Idx := Idx + 1;  --  3 (FC)
      Frame (Idx) := Character'Pos ('0'); Idx := Idx + 1;
      Frame (Idx) := Character'Pos ('0'); Idx := Idx + 1;
      Frame (Idx) := Character'Pos ('0'); Idx := Idx + 1;
      Frame (Idx) := Character'Pos ('0'); Idx := Idx + 1;
      Frame (Idx) := Character'Pos ('0'); Idx := Idx + 1;
      Frame (Idx) := Character'Pos ('0'); Idx := Idx + 1;
      Frame (Idx) := Character'Pos ('0'); Idx := Idx + 1;
      Frame (Idx) := Character'Pos ('A'); Idx := Idx + 1;
      Frame (Idx) := Character'Pos ('F'); Idx := Idx + 1;  --  LRC F2
      Frame (Idx) := Character'Pos ('2'); Idx := Idx + 1;
      Frame (Idx) := Frame_CR; Idx := Idx + 1;
      Frame (Idx) := Frame_LF; Idx := Idx + 1;

      Parse_Frame (Frame, Idx, Slave, PDU, PDU_Len, Result);

      Assert (Result = Success, "Parse should succeed, got " & Result'Image);
      Assert (Slave = 1, "Slave should be 1");
      Assert (PDU_Len = 5, "PDU length should be 5");
      Assert (PDU (0) = 16#03#, "FC should be 0x03");
   end Test_Parse_Valid_Frame;

   --  Test: Parse errors - frame too short
   procedure Test_Parse_Frame_Too_Short (T : in Out Test_Case'Class);
   procedure Test_Parse_Frame_Too_Short (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Frame   : Frame_Buffer := [others => 0];
      PDU     : PDU_Buffer;
      Slave   : Unit_Id;
      PDU_Len : Natural;
      Result  : Status;
   begin
      Frame (0) := Frame_Start;
      Frame (1) := Character'Pos ('0');
      Frame (2) := Character'Pos ('1');
      Frame (3) := Frame_CR;
      Frame (4) := Frame_LF;

      Parse_Frame (Frame, 5, Slave, PDU, PDU_Len, Result);  --  < 9 chars
      Assert (Result = Frame_Error, "Frame too short should fail");
   end Test_Parse_Frame_Too_Short;

   --  Test: Parse errors - missing start character
   procedure Test_Parse_No_Start_Char (T : in Out Test_Case'Class);
   procedure Test_Parse_No_Start_Char (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Frame   : Frame_Buffer := [others => 0];
      PDU     : PDU_Buffer;
      Slave   : Unit_Id;
      PDU_Len : Natural;
      Result  : Status;
   begin
      --  9 chars but no ':' at start
      Frame (0) := Character'Pos ('0');  --  Wrong start
      Frame (1) := Character'Pos ('1');
      Frame (2) := Character'Pos ('0');
      Frame (3) := Character'Pos ('3');
      Frame (4) := Character'Pos ('F');
      Frame (5) := Character'Pos ('C');
      Frame (6) := Frame_CR;
      Frame (7) := Frame_LF;

      Parse_Frame (Frame, 9, Slave, PDU, PDU_Len, Result);
      Assert (Result = Frame_Error, "Missing start char should fail");
   end Test_Parse_No_Start_Char;

   --  Test: Parse errors - bad end characters
   procedure Test_Parse_Bad_End_Chars (T : in Out Test_Case'Class);
   procedure Test_Parse_Bad_End_Chars (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Frame   : Frame_Buffer := [others => 0];
      PDU     : PDU_Buffer;
      Slave   : Unit_Id;
      PDU_Len : Natural;
      Result  : Status;
   begin
      Frame (0) := Frame_Start;
      Frame (1) := Character'Pos ('0');
      Frame (2) := Character'Pos ('1');
      Frame (3) := Character'Pos ('0');
      Frame (4) := Character'Pos ('3');
      Frame (5) := Character'Pos ('F');
      Frame (6) := Character'Pos ('C');
      Frame (7) := Character'Pos ('X');  --  Wrong: not CR
      Frame (8) := Frame_LF;

      Parse_Frame (Frame, 9, Slave, PDU, PDU_Len, Result);
      Assert (Result = Frame_Error, "Bad end chars should fail");
   end Test_Parse_Bad_End_Chars;

   --  Test: Parse errors - odd number of hex chars
   procedure Test_Parse_Odd_Hex_Count (T : in Out Test_Case'Class);
   procedure Test_Parse_Odd_Hex_Count (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Frame   : Frame_Buffer := [others => 0];
      PDU     : PDU_Buffer;
      Slave   : Unit_Id;
      PDU_Len : Natural;
      Result  : Status;
   begin
      --  10 chars total = : + 5 hex + CR LF  (5 hex is odd)
      Frame (0) := Frame_Start;
      Frame (1) := Character'Pos ('0');
      Frame (2) := Character'Pos ('1');
      Frame (3) := Character'Pos ('0');
      Frame (4) := Character'Pos ('3');
      Frame (5) := Character'Pos ('F');
      Frame (6) := Character'Pos ('C');
      Frame (7) := Character'Pos ('A');  --  7 hex chars (odd)
      Frame (8) := Frame_CR;
      Frame (9) := Frame_LF;

      Parse_Frame (Frame, 10, Slave, PDU, PDU_Len, Result);
      Assert (Result = Frame_Error, "Odd hex count should fail");
   end Test_Parse_Odd_Hex_Count;

   --  Test: Parse errors - invalid hex character
   procedure Test_Parse_Invalid_Hex (T : in Out Test_Case'Class);
   procedure Test_Parse_Invalid_Hex (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Frame   : Frame_Buffer := [others => 0];
      PDU     : PDU_Buffer;
      Slave   : Unit_Id;
      PDU_Len : Natural;
      Result  : Status;
   begin
      Frame (0) := Frame_Start;
      Frame (1) := Character'Pos ('0');
      Frame (2) := Character'Pos ('1');
      Frame (3) := Character'Pos ('G');  --  Invalid hex 'G'
      Frame (4) := Character'Pos ('3');
      Frame (5) := Character'Pos ('F');
      Frame (6) := Character'Pos ('C');
      Frame (7) := Frame_CR;
      Frame (8) := Frame_LF;

      Parse_Frame (Frame, 9, Slave, PDU, PDU_Len, Result);
      Assert (Result = Frame_Error, "Invalid hex char should fail");
   end Test_Parse_Invalid_Hex;

   --  Test: Parse errors - bad LRC
   procedure Test_Parse_Bad_LRC (T : in Out Test_Case'Class);
   procedure Test_Parse_Bad_LRC (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Frame   : Frame_Buffer := [others => 0];
      PDU     : PDU_Buffer;
      Slave   : Unit_Id;
      PDU_Len : Natural;
      Result  : Status;
   begin
      --  :0103AA (valid format but wrong LRC)
      Frame (0) := Frame_Start;
      Frame (1) := Character'Pos ('0');
      Frame (2) := Character'Pos ('1');
      Frame (3) := Character'Pos ('0');
      Frame (4) := Character'Pos ('3');
      Frame (5) := Character'Pos ('A');
      Frame (6) := Character'Pos ('A');  --  Wrong LRC
      Frame (7) := Frame_CR;
      Frame (8) := Frame_LF;

      Parse_Frame (Frame, 9, Slave, PDU, PDU_Len, Result);
      Assert (Result = LRC_Error, "Bad LRC should fail");
   end Test_Parse_Bad_LRC;

   --  Test: Parse errors - invalid Unit_Id (> 247)
   procedure Test_Parse_Invalid_Unit_Id (T : in Out Test_Case'Class);
   procedure Test_Parse_Invalid_Unit_Id (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Frame   : Frame_Buffer := [others => 0];
      PDU     : PDU_Buffer;
      Slave   : Unit_Id;
      PDU_Len : Natural;
      Result  : Status;
   begin
      --  Unit ID F8 = 248 > 247, with correct LRC
      --  Data: F8 03, LRC = -(F8+03) mod 256 = 05
      Frame (0) := Frame_Start;
      Frame (1) := Character'Pos ('F');
      Frame (2) := Character'Pos ('8');  --  248
      Frame (3) := Character'Pos ('0');
      Frame (4) := Character'Pos ('3');
      Frame (5) := Character'Pos ('0');
      Frame (6) := Character'Pos ('5');  --  LRC
      Frame (7) := Frame_CR;
      Frame (8) := Frame_LF;

      Parse_Frame (Frame, 9, Slave, PDU, PDU_Len, Result);
      Assert (Result = Frame_Error, "Unit ID > 247 should fail");
   end Test_Parse_Invalid_Unit_Id;

   --  Test: Hex_Nibble with invalid char returns 0
   procedure Test_Hex_Nibble_Invalid (T : in Out Test_Case'Class);
   procedure Test_Hex_Nibble_Invalid (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Result : Byte;
   begin
      --  Hex_To_Byte internally calls Hex_Nibble which returns 0 for invalid
      Result := Hex_To_Byte (Character'Pos ('Z'), Character'Pos ('Z'));
      Assert (Result = 0, "'ZZ' should convert to 0");
   end Test_Hex_Nibble_Invalid;

   --  Test: Round-trip build and parse
   procedure Test_Round_Trip (T : in Out Test_Case'Class);
   procedure Test_Round_Trip (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      PDU_In      : PDU_Buffer := [others => 0];
      PDU_Out     : PDU_Buffer;
      Frame       : Frame_Buffer;
      Frame_Len   : Natural;
      Slave_Out   : Unit_Id;
      PDU_Len_Out : Natural;
      Result      : Status;
   begin
      PDU_In (0) := 16#06#;  --  FC Write Single Register
      PDU_In (1) := 16#00#;
      PDU_In (2) := 16#10#;
      PDU_In (3) := 16#12#;
      PDU_In (4) := 16#34#;

      Build_Frame (Frame, Frame_Len, Slave => 7, PDU => PDU_In, PDU_Length => 5);
      Parse_Frame (Frame, Frame_Len, Slave_Out, PDU_Out, PDU_Len_Out, Result);

      Assert (Result = Success, "Round-trip should succeed");
      Assert (Slave_Out = 7, "Slave should match");
      Assert (PDU_Len_Out = 5, "PDU length should match");
      for I in 0 .. 4 loop
         Assert (PDU_Out (I) = PDU_In (I), "PDU byte" & I'Image & " should match");
      end loop;
   end Test_Round_Trip;

   overriding procedure Register_Tests (T : in Out ASCII_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_Byte_To_Hex'Access, "Byte to Hex");
      Registration.Register_Routine (T, Test_Hex_To_Byte'Access, "Hex to Byte");
      Registration.Register_Routine (T, Test_Is_Hex_Digit'Access, "Is_Hex_Digit");
      Registration.Register_Routine (T, Test_Build_Frame'Access, "Build ASCII frame");
      Registration.Register_Routine (T, Test_Parse_Valid_Frame'Access, "Parse valid frame");
      Registration.Register_Routine (T, Test_Round_Trip'Access, "Round-trip");
      --  Error path tests
      Registration.Register_Routine (T, Test_Parse_Frame_Too_Short'Access, "Frame too short");
      Registration.Register_Routine (T, Test_Parse_No_Start_Char'Access, "Missing start char");
      Registration.Register_Routine (T, Test_Parse_Bad_End_Chars'Access, "Bad end chars");
      Registration.Register_Routine (T, Test_Parse_Odd_Hex_Count'Access, "Odd hex count");
      Registration.Register_Routine (T, Test_Parse_Invalid_Hex'Access, "Invalid hex char");
      Registration.Register_Routine (T, Test_Parse_Bad_LRC'Access, "Bad LRC");
      Registration.Register_Routine (T, Test_Parse_Invalid_Unit_Id'Access, "Invalid Unit ID");
      Registration.Register_Routine (T, Test_Hex_Nibble_Invalid'Access, "Hex_Nibble invalid");
   end Register_Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new ASCII_Test_Case);
      return S;
   end Suite;

end Test_ASCII;
