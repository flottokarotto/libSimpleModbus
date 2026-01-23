--  Ada_Modbus.Energy.Grid_Control - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Energy.Grid_Control
  with SPARK_Mode => On
is

   ------------------------------
   -- Encode_Set_Limit_Request --
   ------------------------------

   procedure Encode_Set_Limit_Request
     (Limit  : Energy.Power_Percent;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
   begin
      --  FC 06: Write Single Register
      Protocol.Encode_Write_Single_Register_Request
        (Address => Power_Limit_Register,
         Value   => Register_Value (Limit),
         Buffer  => Buffer,
         Length  => Length);
   end Encode_Set_Limit_Request;

   ----------------------------------
   -- Encode_Release_Limit_Request --
   ----------------------------------

   procedure Encode_Release_Limit_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
   begin
      Encode_Set_Limit_Request (100, Buffer, Length);
   end Encode_Release_Limit_Request;

   ------------------------------
   -- Encode_Get_Limit_Request --
   ------------------------------

   procedure Encode_Get_Limit_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
   begin
      --  FC 03: Read Holding Registers
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Power_Limit_Register,
         Quantity      => 1,
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Get_Limit_Request;

   -------------------------------
   -- Decode_Get_Limit_Response --
   -------------------------------

   procedure Decode_Get_Limit_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Limit  : out Energy.Power_Percent;
      Result : out Status)
   is
      Values    : Register_Array (0 .. 0);
      Reg_Count : Natural;
   begin
      Limit := 100;  --  Default: no limit

      Protocol.Decode_Read_Registers_Response
        (Buffer    => Buffer,
         Length    => Length,
         Values    => Values,
         Reg_Count => Reg_Count,
         Result    => Result);

      if Result = Success and then Reg_Count >= 1 then
         if Values (0) <= 100 then
            Limit := Energy.Power_Percent (Values (0));
         else
            Limit := 100;
         end if;
      end if;
   end Decode_Get_Limit_Response;

   ------------------------------
   -- Encode_Get_Power_Request --
   ------------------------------

   procedure Encode_Get_Power_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
   begin
      --  FC 03: Read Holding Registers
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Power_Reading_Register,
         Quantity      => 1,
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Get_Power_Request;

   -------------------------------
   -- Decode_Get_Power_Response --
   -------------------------------

   procedure Decode_Get_Power_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Watts  : out Natural;
      Result : out Status)
   is
      Values    : Register_Array (0 .. 0);
      Reg_Count : Natural;
   begin
      Watts := 0;

      Protocol.Decode_Read_Registers_Response
        (Buffer    => Buffer,
         Length    => Length,
         Values    => Values,
         Reg_Count => Reg_Count,
         Result    => Result);

      if Result = Success and then Reg_Count >= 1 then
         Watts := Natural (Values (0)) * Power_Scale;
      end if;
   end Decode_Get_Power_Response;

end Ada_Modbus.Energy.Grid_Control;
