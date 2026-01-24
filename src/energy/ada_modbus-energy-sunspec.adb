--  Ada_Modbus.Energy.SunSpec - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Energy.SunSpec
  with SPARK_Mode => On
is

   -----------------
   -- Apply_Scale --
   -----------------

   function Apply_Scale (Value : Register_Value; SF : Scale_Factor) return Float
   is
   begin
      return Float (Value) * Scale_Multipliers (SF);
   end Apply_Scale;

   ------------------------
   -- Apply_Scale_Signed --
   ------------------------

   function Apply_Scale_Signed
     (Value : Register_Value; SF : Scale_Factor) return Float
   is
      Signed_Value : Integer;
   begin
      --  Convert unsigned 16-bit to signed (two's complement)
      if Value > 32767 then
         Signed_Value := Integer (Value) - 65536;
      else
         Signed_Value := Integer (Value);
      end if;
      return Float (Signed_Value) * Scale_Multipliers (SF);
   end Apply_Scale_Signed;

   -------------------
   -- Decode_String --
   -------------------

   procedure Decode_String
     (Registers : Register_Array;
      Result    : out SunSpec_String;
      Length    : out Natural)
   is
      Idx : Natural := 0;
   begin
      Result := (others => ' ');
      Length := 0;

      for R of Registers loop
         --  High byte first (big-endian)
         declare
            Hi : constant Character := Character'Val (Natural (R / 256) mod 256);
            Lo : constant Character := Character'Val (Natural (R) mod 256);
         begin
            if Idx < 32 and then Hi /= ASCII.NUL then
               Idx := Idx + 1;
               Result (Idx) := Hi;
               Length := Idx;
            end if;
            if Idx < 32 and then Lo /= ASCII.NUL then
               Idx := Idx + 1;
               Result (Idx) := Lo;
               Length := Idx;
            end if;
         end;
      end loop;
   end Decode_String;

   ---------------------------------
   -- Encode_Check_SunSpec_Request --
   ---------------------------------

   procedure Encode_Check_SunSpec_Request
     (Base_Address : Register_Address;
      Buffer       : out Protocol.PDU_Buffer;
      Length       : out Natural)
   is
   begin
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Base_Address,
         Quantity      => 2,
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Check_SunSpec_Request;

   ----------------------------------
   -- Decode_Check_SunSpec_Response --
   ----------------------------------

   procedure Decode_Check_SunSpec_Response
     (Buffer   : Protocol.PDU_Buffer;
      Length   : Natural;
      Is_Valid : out Boolean;
      Result   : out Status)
   is
      Values    : Register_Array (0 .. 1);
      Reg_Count : Natural;
   begin
      Is_Valid := False;

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Reg_Count,
         Response => Result);

      if Result = Success and then Reg_Count = 2 then
         Is_Valid := Values (0) = SunS_ID_High and then
                     Values (1) = SunS_ID_Low;
      end if;
   end Decode_Check_SunSpec_Response;

   -------------------------------------
   -- Encode_Read_Model_Header_Request --
   -------------------------------------

   procedure Encode_Read_Model_Header_Request
     (Base_Address : Register_Address;
      Offset       : Register_Address;
      Buffer       : out Protocol.PDU_Buffer;
      Length       : out Natural)
   is
   begin
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Base_Address + Offset,
         Quantity      => 2,
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Read_Model_Header_Request;

   ---------------------------------
   -- Decode_Model_Header_Response --
   ---------------------------------

   procedure Decode_Model_Header_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Header : out Model_Header;
      Result : out Status)
   is
      Values    : Register_Array (0 .. 1);
      Reg_Count : Natural;
   begin
      Header := (ID => 0, Length => 0);

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Reg_Count,
         Response => Result);

      if Result = Success and then Reg_Count = 2 then
         Header.ID     := Model_ID (Values (0));
         Header.Length := Model_Length (Values (1));
      end if;
   end Decode_Model_Header_Response;

end Ada_Modbus.Energy.SunSpec;
