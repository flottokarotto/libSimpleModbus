--  Ada_Modbus.Energy.SG_Ready - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Energy.SG_Ready
  with SPARK_Mode => On
is

   --  State to numeric value mapping
   State_Values : constant array (Operating_State) of Register_Value :=
     [Blocked     => (if Zero_Based then 0 else 1),
      Normal      => (if Zero_Based then 1 else 2),
      Recommended => (if Zero_Based then 2 else 3),
      Forced      => (if Zero_Based then 3 else 4)];

   -------------------------
   -- To_Register_Value --
   -------------------------

   function To_Register_Value (State : Operating_State) return Register_Value is
   begin
      return State_Values (State);
   end To_Register_Value;

   ---------------------------
   -- From_Register_Value --
   ---------------------------

   function From_Register_Value (Value : Register_Value) return Operating_State is
      Adjusted : constant Register_Value :=
        (if Zero_Based then Value else Value - 1);
   begin
      case Adjusted is
         when 0      => return Blocked;
         when 1      => return Normal;
         when 2      => return Recommended;
         when 3      => return Forced;
         when others => return Normal;  --  Default for invalid values
      end case;
   end From_Register_Value;

   -------------------------------
   -- Encode_Set_State_Request --
   -------------------------------

   procedure Encode_Set_State_Request
     (State  : Operating_State;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
      Value : constant Register_Value := To_Register_Value (State);
   begin
      --  FC 06: Write Single Register
      Protocol.Encode_Write_Single_Register_Request
        (Address => State_Register,
         Value   => Value,
         Buffer  => Buffer,
         Length  => Length);
   end Encode_Set_State_Request;

   -------------------------------
   -- Encode_Get_State_Request --
   -------------------------------

   procedure Encode_Get_State_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
   begin
      --  FC 03: Read Holding Registers (1 register)
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => State_Register,
         Quantity      => 1,
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Get_State_Request;

   --------------------------------
   -- Decode_Get_State_Response --
   --------------------------------

   procedure Decode_Get_State_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      State  : out Operating_State;
      Result : out Status)
   is
      Values     : Register_Array (0 .. 0);
      Reg_Count  : Natural;
   begin
      State := Normal;  --  Default

      Protocol.Decode_Read_Registers_Response
        (Buffer    => Buffer,
         Length    => Length,
         Values    => Values,
         Reg_Count => Reg_Count,
         Result    => Result);

      if Result = Success and then Reg_Count >= 1 then
         State := From_Register_Value (Values (0));
      end if;
   end Decode_Get_State_Response;

end Ada_Modbus.Energy.SG_Ready;
