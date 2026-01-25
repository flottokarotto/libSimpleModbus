--  Test_Utilities - Utilities unit tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Utilities; use Ada_Modbus.Utilities;

package body Test_Utilities is

   type Utilities_Test_Case is new Test_Case with null record;

   overriding function Name (T : Utilities_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("Utilities Tests"));

   overriding procedure Register_Tests (T : in out Utilities_Test_Case);

   --  Test: To_Big_Endian conversion
   procedure Test_To_Big_Endian (T : in Out Test_Case'Class);
   procedure Test_To_Big_Endian (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Result : Byte_Array (0 .. 1);
   begin
      Result := To_Big_Endian (16#1234#);
      Assert (Result (0) = 16#12#, "High byte should be 0x12");
      Assert (Result (1) = 16#34#, "Low byte should be 0x34");

      Result := To_Big_Endian (16#0000#);
      Assert (Result (0) = 16#00# and Result (1) = 16#00#, "Zero value");

      Result := To_Big_Endian (16#FFFF#);
      Assert (Result (0) = 16#FF# and Result (1) = 16#FF#, "Max value");
   end Test_To_Big_Endian;

   --  Test: From_Big_Endian (two bytes)
   procedure Test_From_Big_Endian_Bytes (T : in Out Test_Case'Class);
   procedure Test_From_Big_Endian_Bytes (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Result : Register_Value;
   begin
      Result := From_Big_Endian (16#12#, 16#34#);
      Assert (Result = 16#1234#, "Should combine to 0x1234");

      Result := From_Big_Endian (16#00#, 16#00#);
      Assert (Result = 16#0000#, "Zero value");

      Result := From_Big_Endian (16#FF#, 16#FF#);
      Assert (Result = 16#FFFF#, "Max value");
   end Test_From_Big_Endian_Bytes;

   --  Test: From_Big_Endian (array)
   procedure Test_From_Big_Endian_Array (T : in Out Test_Case'Class);
   procedure Test_From_Big_Endian_Array (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Data   : constant Byte_Array := [16#AB#, 16#CD#];
      Result : Register_Value;
   begin
      Result := From_Big_Endian (Data);
      Assert (Result = 16#ABCD#, "Should combine to 0xABCD");
   end Test_From_Big_Endian_Array;

   --  Test: High_Byte extraction
   procedure Test_High_Byte (T : in Out Test_Case'Class);
   procedure Test_High_Byte (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (High_Byte (16#1234#) = 16#12#, "High byte of 0x1234");
      Assert (High_Byte (16#FF00#) = 16#FF#, "High byte of 0xFF00");
      Assert (High_Byte (16#00FF#) = 16#00#, "High byte of 0x00FF");
   end Test_High_Byte;

   --  Test: Low_Byte extraction
   procedure Test_Low_Byte (T : in Out Test_Case'Class);
   procedure Test_Low_Byte (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (Low_Byte (16#1234#) = 16#34#, "Low byte of 0x1234");
      Assert (Low_Byte (16#FF00#) = 16#00#, "Low byte of 0xFF00");
      Assert (Low_Byte (16#00FF#) = 16#FF#, "Low byte of 0x00FF");
   end Test_Low_Byte;

   --  Test: Round-trip conversion
   procedure Test_Round_Trip (T : in Out Test_Case'Class);
   procedure Test_Round_Trip (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Original : constant Register_Value := 16#BEEF#;
      Encoded  : Byte_Array (0 .. 1);
      Decoded  : Register_Value;
   begin
      Encoded := To_Big_Endian (Original);
      Decoded := From_Big_Endian (Encoded);
      Assert (Decoded = Original, "Round-trip should preserve value");
   end Test_Round_Trip;

   --  Test: 32-bit Big-Endian (ABCD)
   procedure Test_32bit_Big_Endian (T : in Out Test_Case'Class);
   procedure Test_32bit_Big_Endian (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      use type Interfaces.Unsigned_32;
      Result : Interfaces.Unsigned_32;
   begin
      --  0x12345678: High=0x1234, Low=0x5678
      Result := To_Unsigned_32 (16#1234#, 16#5678#, Big_Endian);
      Assert (Result = 16#12345678#, "ABCD: 0x1234, 0x5678 -> 0x12345678");

      Result := To_Unsigned_32 (16#0000#, 16#0001#, Big_Endian);
      Assert (Result = 16#00000001#, "ABCD: 0x0000, 0x0001 -> 0x00000001");

      Result := To_Unsigned_32 (16#FFFF#, 16#FFFF#, Big_Endian);
      Assert (Result = 16#FFFFFFFF#, "ABCD: max value");
   end Test_32bit_Big_Endian;

   --  Test: 32-bit Little-Endian (DCBA) - fully byte-reversed
   procedure Test_32bit_Little_Endian (T : in Out Test_Case'Class);
   procedure Test_32bit_Little_Endian (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      use type Interfaces.Unsigned_32;
      Result : Interfaces.Unsigned_32;
   begin
      --  DCBA: bytes fully reversed
      --  For 0x12345678: D=0x78, C=0x56, B=0x34, A=0x12
      --  Input High=0x7856 (DC), Low=0x3412 (BA) -> 0x12345678
      Result := To_Unsigned_32 (16#7856#, 16#3412#, Little_Endian);
      Assert (Result = 16#12345678#, "DCBA: 0x7856, 0x3412 -> 0x12345678");

      --  Additional test with different value
      --  For 0xDEADBEEF: D=0xEF, C=0xBE, B=0xAD, A=0xDE
      --  Input High=0xEFBE (DC), Low=0xADDE (BA) -> 0xDEADBEEF
      Result := To_Unsigned_32 (16#EFBE#, 16#ADDE#, Little_Endian);
      Assert (Result = 16#DEADBEEF#, "DCBA: 0xEFBE, 0xADDE -> 0xDEADBEEF");
   end Test_32bit_Little_Endian;

   --  Test: 32-bit Mid-Big-Endian (BADC) - bytes swapped within words
   procedure Test_32bit_Mid_Big_Endian (T : in Out Test_Case'Class);
   procedure Test_32bit_Mid_Big_Endian (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      use type Interfaces.Unsigned_32;
      Result : Interfaces.Unsigned_32;
   begin
      --  BADC: bytes swapped within each word
      --  For 0x12345678: B=0x34, A=0x12, D=0x78, C=0x56
      --  Input High=0x3412 (BA), Low=0x7856 (DC) -> 0x12345678
      Result := To_Unsigned_32 (16#3412#, 16#7856#, Mid_Big_Endian);
      Assert (Result = 16#12345678#, "BADC: 0x3412, 0x7856 -> 0x12345678");

      --  Additional test with different value
      --  For 0xDEADBEEF: B=0xAD, A=0xDE, D=0xEF, C=0xBE
      --  Input High=0xADDE (BA), Low=0xEFBE (DC) -> 0xDEADBEEF
      Result := To_Unsigned_32 (16#ADDE#, 16#EFBE#, Mid_Big_Endian);
      Assert (Result = 16#DEADBEEF#, "BADC: 0xADDE, 0xEFBE -> 0xDEADBEEF");
   end Test_32bit_Mid_Big_Endian;

   --  Test: 32-bit Mid-Little-Endian (CDAB) - word-swapped
   procedure Test_32bit_Mid_Little_Endian (T : in Out Test_Case'Class);
   procedure Test_32bit_Mid_Little_Endian (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      use type Interfaces.Unsigned_32;
      Result : Interfaces.Unsigned_32;
   begin
      --  CDAB: Low word first, high word second
      --  For 0x12345678: C=0x56, D=0x78, A=0x12, B=0x34
      --  Input High=0x5678 (CD), Low=0x1234 (AB) -> 0x12345678
      Result := To_Unsigned_32 (16#5678#, 16#1234#, Mid_Little_Endian);
      Assert (Result = 16#12345678#, "CDAB: 0x5678, 0x1234 -> 0x12345678");

      --  Additional test with different value
      --  For 0xDEADBEEF: C=0xBE, D=0xEF, A=0xDE, B=0xAD
      --  Input High=0xBEEF (CD), Low=0xDEAD (AB) -> 0xDEADBEEF
      Result := To_Unsigned_32 (16#BEEF#, 16#DEAD#, Mid_Little_Endian);
      Assert (Result = 16#DEADBEEF#, "CDAB: 0xBEEF, 0xDEAD -> 0xDEADBEEF");
   end Test_32bit_Mid_Little_Endian;

   --  Test: 32-bit from Register_Array
   procedure Test_32bit_From_Array (T : in Out Test_Case'Class);
   procedure Test_32bit_From_Array (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      use type Interfaces.Unsigned_32;
      Regs   : constant Register_Array := [16#ABCD#, 16#EF01#];
      Result : Interfaces.Unsigned_32;
   begin
      Result := Registers_To_Unsigned_32 (Regs, Big_Endian);
      Assert (Result = 16#ABCDEF01#, "Array ABCD: 0xABCDEF01");

      Result := Registers_To_Unsigned_32 (Regs, Mid_Little_Endian);
      Assert (Result = 16#EF01ABCD#, "Array CDAB: 0xEF01ABCD");
   end Test_32bit_From_Array;

   --  Test: 32-bit round-trip for all word orders
   procedure Test_32bit_Round_Trip (T : in Out Test_Case'Class);
   procedure Test_32bit_Round_Trip (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      use type Interfaces.Unsigned_32;
      Original : constant Interfaces.Unsigned_32 := 16#DEADBEEF#;
      High, Low : Register_Value;
      Decoded  : Interfaces.Unsigned_32;
   begin
      --  Test all 4 word orders for round-trip consistency
      From_Unsigned_32 (Original, High, Low, Big_Endian);
      Decoded := To_Unsigned_32 (High, Low, Big_Endian);
      Assert (Decoded = Original, "ABCD round-trip");

      From_Unsigned_32 (Original, High, Low, Little_Endian);
      Decoded := To_Unsigned_32 (High, Low, Little_Endian);
      Assert (Decoded = Original, "DCBA round-trip");

      From_Unsigned_32 (Original, High, Low, Mid_Big_Endian);
      Decoded := To_Unsigned_32 (High, Low, Mid_Big_Endian);
      Assert (Decoded = Original, "BADC round-trip");

      From_Unsigned_32 (Original, High, Low, Mid_Little_Endian);
      Decoded := To_Unsigned_32 (High, Low, Mid_Little_Endian);
      Assert (Decoded = Original, "CDAB round-trip");
   end Test_32bit_Round_Trip;

   --  Test: From_Unsigned_32 produces correct word values for all orders
   procedure Test_From_Unsigned_32_All_Orders (T : in Out Test_Case'Class);
   procedure Test_From_Unsigned_32_All_Orders (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      use type Interfaces.Unsigned_32;
      --  Test value 0x12345678: A=0x12, B=0x34, C=0x56, D=0x78
      Value : constant Interfaces.Unsigned_32 := 16#12345678#;
      High, Low : Register_Value;
   begin
      --  Big-Endian (ABCD): High=AB=0x1234, Low=CD=0x5678
      From_Unsigned_32 (Value, High, Low, Big_Endian);
      Assert (High = 16#1234#, "ABCD High should be 0x1234");
      Assert (Low = 16#5678#, "ABCD Low should be 0x5678");

      --  Little-Endian (DCBA): High=DC=0x7856, Low=BA=0x3412
      From_Unsigned_32 (Value, High, Low, Little_Endian);
      Assert (High = 16#7856#, "DCBA High should be 0x7856");
      Assert (Low = 16#3412#, "DCBA Low should be 0x3412");

      --  Mid-Big-Endian (BADC): High=BA=0x3412, Low=DC=0x7856
      From_Unsigned_32 (Value, High, Low, Mid_Big_Endian);
      Assert (High = 16#3412#, "BADC High should be 0x3412");
      Assert (Low = 16#7856#, "BADC Low should be 0x7856");

      --  Mid-Little-Endian (CDAB): High=CD=0x5678, Low=AB=0x1234
      From_Unsigned_32 (Value, High, Low, Mid_Little_Endian);
      Assert (High = 16#5678#, "CDAB High should be 0x5678");
      Assert (Low = 16#1234#, "CDAB Low should be 0x1234");
   end Test_From_Unsigned_32_All_Orders;

   overriding procedure Register_Tests (T : in Out Utilities_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_To_Big_Endian'Access, "To_Big_Endian");
      Registration.Register_Routine (T, Test_From_Big_Endian_Bytes'Access, "From_Big_Endian (bytes)");
      Registration.Register_Routine (T, Test_From_Big_Endian_Array'Access, "From_Big_Endian (array)");
      Registration.Register_Routine (T, Test_High_Byte'Access, "High_Byte");
      Registration.Register_Routine (T, Test_Low_Byte'Access, "Low_Byte");
      Registration.Register_Routine (T, Test_Round_Trip'Access, "Round-trip conversion");
      Registration.Register_Routine (T, Test_32bit_Big_Endian'Access, "32-bit Big-Endian (ABCD)");
      Registration.Register_Routine (T, Test_32bit_Little_Endian'Access, "32-bit Little-Endian (DCBA)");
      Registration.Register_Routine (T, Test_32bit_Mid_Big_Endian'Access, "32-bit Mid-Big-Endian (BADC)");
      Registration.Register_Routine (T, Test_32bit_Mid_Little_Endian'Access, "32-bit Mid-Little-Endian (CDAB)");
      Registration.Register_Routine (T, Test_32bit_From_Array'Access, "32-bit from Register_Array");
      Registration.Register_Routine (T, Test_32bit_Round_Trip'Access, "32-bit round-trip (all orders)");
      Registration.Register_Routine (T, Test_From_Unsigned_32_All_Orders'Access, "From_Unsigned_32 (all orders)");
   end Register_Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new Utilities_Test_Case);
      return S;
   end Suite;

end Test_Utilities;
