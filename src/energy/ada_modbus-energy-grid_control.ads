--  Ada_Modbus.Energy.Grid_Control - §14a Power Limitation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Implements power limitation control as required by §14a EnWG (Germany).
--  Grid operators can reduce power consumption of controllable devices
--  (heat pumps, EV chargers, batteries) during grid congestion.
--
--  Typical use: Energy Management System (EMS) receives §14a signals
--  and translates them to Modbus commands for connected devices.

with Ada_Modbus.Protocol;

generic
   --  Modbus register for power limit (0-100%)
   Power_Limit_Register : Register_Address;

   --  Optional: Register to read current power consumption (Watts)
   Power_Reading_Register : Register_Address := 0;

   --  Power reading scale factor (e.g., 10 = value in 0.1W units)
   Power_Scale : Positive := 1;

package Ada_Modbus.Energy.Grid_Control
  with SPARK_Mode => On
is

   --  Set power limit as percentage (0 = off, 100 = full power)
   procedure Encode_Set_Limit_Request
     (Limit  : Energy.Power_Percent;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Release power limit (set to 100%)
   procedure Encode_Release_Limit_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Read current power limit
   procedure Encode_Get_Limit_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Decode power limit response
   procedure Decode_Get_Limit_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Limit  : out Energy.Power_Percent;
      Result : out Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

   --  Read current power consumption (if Power_Reading_Register configured)
   procedure Encode_Get_Power_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Decode power reading response (returns Watts)
   procedure Decode_Get_Power_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Watts  : out Natural;
      Result : out Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

end Ada_Modbus.Energy.Grid_Control;
