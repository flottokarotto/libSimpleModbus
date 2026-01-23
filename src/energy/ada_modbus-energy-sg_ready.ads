--  Ada_Modbus.Energy.SG_Ready - Heat Pump Control (SG-Ready Standard)
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  SG-Ready is a German standard for smart grid integration of heat pumps.
--  It defines 4 operating states controlled via 2 switching contacts:
--
--  | State | Contact 1 | Contact 2 | Description                    |
--  |-------|-----------|-----------|--------------------------------|
--  |   1   |    ON     |    OFF    | Blocked (EVU-Sperre)           |
--  |   2   |    OFF    |    OFF    | Normal operation               |
--  |   3   |    OFF    |    ON     | Recommended (e.g., PV surplus) |
--  |   4   |    ON     |    ON     | Forced max power               |
--
--  This package provides a generic interface for Modbus-controlled heat pumps.
--  Register addresses are manufacturer-specific and must be configured.

with Ada_Modbus.Protocol;

generic
   --  Modbus register address for SG-Ready state
   --  Some devices use a single register (0-3 or 1-4)
   --  Others use two coils for the two contacts
   State_Register : Register_Address;

   --  Whether the device uses 0-based (0-3) or 1-based (1-4) states
   Zero_Based : Boolean := False;

package Ada_Modbus.Energy.SG_Ready
  with SPARK_Mode => On
is

   --  SG-Ready operating states
   type Operating_State is
     (Blocked,      --  State 1: EVU lock, reduce to minimum
      Normal,       --  State 2: Normal operation
      Recommended,  --  State 3: Switch-on recommendation (PV surplus)
      Forced);      --  State 4: Forced run at maximum power

   --  Convert operating state to register value
   function To_Register_Value (State : Operating_State) return Register_Value;

   --  Convert register value to operating state
   function From_Register_Value (Value : Register_Value) return Operating_State;

   --  Encode a Set_State request into PDU buffer
   procedure Encode_Set_State_Request
     (State  : Operating_State;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Encode a Get_State request into PDU buffer
   procedure Encode_Get_State_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Decode a Get_State response from PDU buffer
   procedure Decode_Get_State_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      State  : out Operating_State;
      Result : out Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

end Ada_Modbus.Energy.SG_Ready;
