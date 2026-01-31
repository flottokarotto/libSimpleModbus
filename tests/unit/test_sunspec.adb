--  Test_SunSpec - SunSpec energy package tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Protocol; use Ada_Modbus.Protocol;
with Ada_Modbus.Energy.SunSpec; use Ada_Modbus.Energy.SunSpec;
with Ada_Modbus.Energy.SunSpec.Inverter; use Ada_Modbus.Energy.SunSpec.Inverter;
with Ada_Modbus.Energy.SunSpec.Common; use Ada_Modbus.Energy.SunSpec.Common;
with Ada_Modbus.Energy.SunSpec.Meter; use Ada_Modbus.Energy.SunSpec.Meter;
with Ada_Modbus.Energy.SunSpec.Storage; use Ada_Modbus.Energy.SunSpec.Storage;
with Ada_Modbus.Energy.SunSpec.Nameplate; use Ada_Modbus.Energy.SunSpec.Nameplate;
with Ada_Modbus.Energy.SunSpec.Settings; use Ada_Modbus.Energy.SunSpec.Settings;
with Ada_Modbus.Energy.SunSpec.DER; use Ada_Modbus.Energy.SunSpec.DER;
with Ada_Modbus.Energy.SunSpec.Battery; use Ada_Modbus.Energy.SunSpec.Battery;

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

   --  Test: Signed scale factor application (for temperatures)
   procedure Test_Apply_Scale_Signed (T : in Out Test_Case'Class);
   procedure Test_Apply_Scale_Signed (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      --  Positive value (47 degrees with SF=0)
      Assert (Apply_Scale_Signed (47, 0) = 47.0,
              "Positive value should work like unsigned");

      --  Positive value with scale factor
      Assert (Apply_Scale_Signed (470, -1) = 47.0,
              "Positive value with negative SF");

      --  Negative value: -5 degrees stored as 65531 (two's complement)
      --  65536 - 5 = 65531
      Assert (Apply_Scale_Signed (65531, 0) = -5.0,
              "Negative value (two's complement) should be -5");

      --  Negative value with scale factor: -1.5 degrees
      --  -15 as unsigned = 65521, SF=-1 -> -1.5
      Assert (Apply_Scale_Signed (65521, -1) = -1.5,
              "Negative value with SF=-1");

      --  Edge case: exactly 32767 (max positive int16)
      Assert (Apply_Scale_Signed (32767, 0) = 32767.0,
              "Max positive int16 should stay positive");

      --  Edge case: 32768 (min negative int16 = -32768)
      Assert (Apply_Scale_Signed (32768, 0) = -32768.0,
              "32768 as unsigned should be -32768 signed");
   end Test_Apply_Scale_Signed;

   --  Test: Inverter AC measurements decode
   procedure Test_Inverter_AC_Decode (T : in Out Test_Case'Class);
   procedure Test_Inverter_AC_Decode (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Current, Voltage, Power, Frequency : Float;
      Result : Status;
   begin
      --  Build simulated Modbus response for 16 registers
      --  FC=03, ByteCount=32, then 16 registers (big-endian)
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 32;      --  Byte count (16 regs * 2)

      --  Register 0 (offset 2): AC Current = 1234 (12.34A with SF=-2)
      Buffer (2) := 16#04#; Buffer (3) := 16#D2#;  -- 1234

      --  Registers 1-3: Phase currents (skip)
      --  Register 4 (offset 10): Current SF = -2 (0xFFFE as two's complement)
      Buffer (10) := 16#FF#; Buffer (11) := 16#FE#;  -- -2

      --  Registers 5-7: Line-line voltages (skip)
      --  Register 8 (offset 18): Voltage AN = 2300 (230.0V with SF=-1)
      Buffer (18) := 16#08#; Buffer (19) := 16#FC#;  -- 2300

      --  Registers 9-10: Voltage BN, CN (skip)
      --  Register 11 (offset 24): Voltage SF = -1 (0xFFFF)
      Buffer (24) := 16#FF#; Buffer (25) := 16#FF#;  -- -1

      --  Register 12 (offset 26): AC Power = 5000 (5000W with SF=0)
      Buffer (26) := 16#13#; Buffer (27) := 16#88#;  -- 5000

      --  Register 13 (offset 28): Power SF = 0
      Buffer (28) := 16#00#; Buffer (29) := 16#00#;  -- 0

      --  Register 14 (offset 30): Frequency = 5000 (50.00Hz with SF=-2)
      Buffer (30) := 16#13#; Buffer (31) := 16#88#;  -- 5000

      --  Register 15 (offset 32): Frequency SF = -2
      Buffer (32) := 16#FF#; Buffer (33) := 16#FE#;  -- -2

      Decode_AC_Measurements_Response
        (Buffer       => Buffer,
         Length       => 34,
         Current_A    => Current,
         Voltage_V    => Voltage,
         Power_W      => Power,
         Frequency_Hz => Frequency,
         Result       => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (abs (Current - 12.34) < 0.01, "Current should be ~12.34A");
      Assert (abs (Voltage - 230.0) < 0.1, "Voltage should be ~230V");
      Assert (abs (Power - 5000.0) < 0.1, "Power should be 5000W");
      Assert (abs (Frequency - 50.0) < 0.01, "Frequency should be 50Hz");
   end Test_Inverter_AC_Decode;

   --  Test: Inverter DC measurements decode
   procedure Test_Inverter_DC_Decode (T : in Out Test_Case'Class);
   procedure Test_Inverter_DC_Decode (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Current, Voltage, Power : Float;
      Result : Status;
   begin
      --  Build response for 6 registers
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 12;      --  Byte count (6 regs * 2)

      --  Register 0: DC Current = 850 (8.50A with SF=-2)
      Buffer (2) := 16#03#; Buffer (3) := 16#52#;  -- 850

      --  Register 1: Current SF = -2
      Buffer (4) := 16#FF#; Buffer (5) := 16#FE#;  -- -2

      --  Register 2: DC Voltage = 4500 (450.0V with SF=-1)
      Buffer (6) := 16#11#; Buffer (7) := 16#94#;  -- 4500

      --  Register 3: Voltage SF = -1
      Buffer (8) := 16#FF#; Buffer (9) := 16#FF#;  -- -1

      --  Register 4: DC Power = 3825 (3825W with SF=0)
      Buffer (10) := 16#0E#; Buffer (11) := 16#F1#;  -- 3825

      --  Register 5: Power SF = 0
      Buffer (12) := 16#00#; Buffer (13) := 16#00#;  -- 0

      Decode_DC_Measurements_Response
        (Buffer    => Buffer,
         Length    => 14,
         Current_A => Current,
         Voltage_V => Voltage,
         Power_W   => Power,
         Result    => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (abs (Current - 8.5) < 0.01, "DC Current should be ~8.5A");
      Assert (abs (Voltage - 450.0) < 0.1, "DC Voltage should be ~450V");
      Assert (abs (Power - 3825.0) < 0.1, "DC Power should be 3825W");
   end Test_Inverter_DC_Decode;

   --  Test: Inverter energy decode (32-bit value)
   procedure Test_Inverter_Energy_Decode (T : in Out Test_Case'Class);
   procedure Test_Inverter_Energy_Decode (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Energy : Float;
      Result : Status;
   begin
      --  Build response for 3 registers (32-bit energy + SF)
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 6;       --  Byte count (3 regs * 2)

      --  Energy = 12345678 Wh (0x00BC614E)
      --  High word at reg 0, low word at reg 1
      Buffer (2) := 16#00#; Buffer (3) := 16#BC#;  -- High: 0x00BC = 188
      Buffer (4) := 16#61#; Buffer (5) := 16#4E#;  -- Low:  0x614E = 24910
      --  188 * 65536 + 24910 = 12345678

      --  SF = 0
      Buffer (6) := 16#00#; Buffer (7) := 16#00#;

      Decode_Energy_Response
        (Buffer    => Buffer,
         Length    => 8,
         Energy_Wh => Energy,
         Result    => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (abs (Energy - 12345678.0) < 1.0, "Energy should be 12345678 Wh");
   end Test_Inverter_Energy_Decode;

   --  Test: Inverter state decode
   procedure Test_Inverter_State_Decode (T : in Out Test_Case'Class);
   procedure Test_Inverter_State_Decode (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      State  : Inverter_State;
      Result : Status;
   begin
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 4;       --  Byte count

      --  State = 4 (Running/MPPT)
      Buffer (2) := 16#00#; Buffer (3) := 16#04#;

      --  Vendor state (ignored)
      Buffer (4) := 16#00#; Buffer (5) := 16#00#;

      Decode_State_Response
        (Buffer => Buffer,
         Length => 6,
         State  => State,
         Result => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (State = Running, "State should be Running (MPPT)");

      --  Test Fault state (7)
      Buffer (3) := 16#07#;
      Decode_State_Response (Buffer, 6, State, Result);
      Assert (State = Fault, "State should be Fault");

      --  Test Sleeping state (2)
      Buffer (3) := 16#02#;
      Decode_State_Response (Buffer, 6, State, Result);
      Assert (State = Sleeping, "State should be Sleeping");
   end Test_Inverter_State_Decode;

   --  Test: Common model request encoding
   procedure Test_Common_Model_Requests (T : in Out Test_Case'Class);
   procedure Test_Common_Model_Requests (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      --  Test manufacturer request (base=40000, model_offset=2)
      --  Should read from 40000 + 2 + 2 = 40004
      Encode_Read_Manufacturer_Request
        (Base_Address => 40000,
         Model_Offset => 2,
         Buffer       => Buffer,
         Length       => Length);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#03#, "FC should be 0x03");
      --  Address 40004 = 0x9C44
      Assert (Buffer (1) = 16#9C#, "Address high");
      Assert (Buffer (2) = 16#44#, "Address low");
      --  Quantity = 16 (32 chars = 16 registers)
      Assert (Buffer (4) = 16#10#, "Quantity should be 16");
   end Test_Common_Model_Requests;

   --  Test: To_Scale_Factor conversion
   procedure Test_To_Scale_Factor (T : in Out Test_Case'Class);
   procedure Test_To_Scale_Factor (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      --  Positive scale factors
      Assert (To_Scale_Factor (0) = 0, "SF 0 should be 0");
      Assert (To_Scale_Factor (1) = 1, "SF 1 should be 1");
      Assert (To_Scale_Factor (10) = 10, "SF 10 should be 10");

      --  Negative scale factors (two's complement)
      --  -1 = 65535, -2 = 65534, etc.
      Assert (To_Scale_Factor (65535) = -1, "SF 0xFFFF should be -1");
      Assert (To_Scale_Factor (65534) = -2, "SF 0xFFFE should be -2");
      Assert (To_Scale_Factor (65526) = -10, "SF 0xFFF6 should be -10");

      --  Out of range values should clamp to 0
      Assert (To_Scale_Factor (11) = 0, "SF 11 should clamp to 0");
      Assert (To_Scale_Factor (100) = 0, "SF 100 should clamp to 0");
      Assert (To_Scale_Factor (65525) = 0, "SF -11 should clamp to 0");
   end Test_To_Scale_Factor;

   --  Test: To_Signed_16 conversion
   procedure Test_To_Signed_16 (T : in Out Test_Case'Class);
   procedure Test_To_Signed_16 (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      --  Positive values
      Assert (To_Signed_16 (0) = 0, "0 should be 0");
      Assert (To_Signed_16 (100) = 100, "100 should be 100");
      Assert (To_Signed_16 (32767) = 32767, "32767 should be 32767");

      --  Negative values (two's complement)
      Assert (To_Signed_16 (65535) = -1, "0xFFFF should be -1");
      Assert (To_Signed_16 (65436) = -100, "65436 should be -100");
      Assert (To_Signed_16 (32768) = -32768, "0x8000 should be -32768");
   end Test_To_Signed_16;

   --  Test: Is_Implemented checks
   procedure Test_Is_Implemented (T : in Out Test_Case'Class);
   procedure Test_Is_Implemented (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      --  Valid values should be implemented
      Assert (Is_Implemented (0), "0 should be implemented");
      Assert (Is_Implemented (1234), "1234 should be implemented");
      Assert (Is_Implemented (32766), "32766 should be implemented");

      --  Not implemented markers for uint16
      Assert (not Is_Implemented (16#FFFF#), "0xFFFF should not be implemented");
      Assert (not Is_Implemented (16#7FFF#), "0x7FFF should not be implemented (some devices use this)");

      --  Int16 checks
      Assert (Is_Implemented_Int16 (0), "0 should be implemented (int16)");
      Assert (Is_Implemented_Int16 (1234), "1234 should be implemented (int16)");
      Assert (not Is_Implemented_Int16 (16#8000#), "0x8000 should not be implemented (int16)");
      Assert (not Is_Implemented_Int16 (16#FFFF#), "0xFFFF should not be implemented (int16)");
   end Test_Is_Implemented;

   ---------------------------------------------------------------------------
   --  Meter package tests
   ---------------------------------------------------------------------------

   procedure Test_Meter_To_Type (T : in out Test_Case'Class);
   procedure Test_Meter_To_Type (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (To_Meter_Type (201) = Meter_1P, "Model 201 = Single Phase");
      Assert (To_Meter_Type (202) = Meter_SP, "Model 202 = Split Phase");
      Assert (To_Meter_Type (203) = Meter_3P_Wye, "Model 203 = Wye 3P");
      Assert (To_Meter_Type (204) = Meter_3P_Delta, "Model 204 = Delta 3P");
      Assert (To_Meter_Type (999) = Meter_1P, "Unknown model = default");
   end Test_Meter_To_Type;

   procedure Test_Meter_Scale_Factors (T : in Out Test_Case'Class);
   procedure Test_Meter_Scale_Factors (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Regs : Register_Array (0 .. 54) := [others => 0];
      SF   : Meter_Scale_Factors;
   begin
      --  Set scale factors in the register array
      --  The decode functions use (Reg_X - 2) as the index (skipping 2-reg header)
      Regs (Meter.Reg_A_SF - 2) := 65534;   --  -2
      Regs (Meter.Reg_V_SF - 2) := 65535;   --  -1
      Regs (Meter.Reg_Hz_SF - 2) := 65534;  --  -2
      Regs (Meter.Reg_W_SF - 2) := 0;       --  0
      Regs (Meter.Reg_VA_SF - 2) := 0;
      Regs (Meter.Reg_VAR_SF - 2) := 0;
      Regs (Meter.Reg_PF_SF - 2) := 65534;  --  -2
      Regs (Meter.Reg_TotWh_SF - 2) := 0;

      Decode_Meter_Scale_Factors (Regs, SF);

      Assert (SF.A_SF = -2, "Current SF should be -2");
      Assert (SF.V_SF = -1, "Voltage SF should be -1");
      Assert (SF.Hz_SF = -2, "Frequency SF should be -2");
      Assert (SF.W_SF = 0, "Power SF should be 0");
   end Test_Meter_Scale_Factors;

   procedure Test_Meter_Totals (T : in Out Test_Case'Class);
   procedure Test_Meter_Totals (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Regs : Register_Array (0 .. 54) := [others => 0];
      SF   : constant Meter_Scale_Factors := (A_SF => -2, V_SF => -1, Hz_SF => -2,
                                              W_SF => 0, VA_SF => 0, VAR_SF => 0,
                                              PF_SF => -2, Wh_SF => 0);
      Data : Meter_Data;
   begin
      --  Set up test registers (Reg_X - 2 to skip header)
      Regs (Meter.Reg_A - 2) := 1000;      --  10.00 A (SF=-2)
      Regs (Meter.Reg_PhV - 2) := 2300;    --  230.0 V (SF=-1)
      Regs (Meter.Reg_Hz - 2) := 5000;     --  50.00 Hz (SF=-2)
      Regs (Meter.Reg_W - 2) := 2300;      --  2300 W (SF=0)
      Regs (Meter.Reg_VA - 2) := 2400;     --  2400 VA
      Regs (Meter.Reg_VAR - 2) := 500;     --  500 var
      Regs (Meter.Reg_PF - 2) := 9500;     --  0.95 PF (SF=-2)

      Decode_Meter_Totals (Regs, SF, Meter_3P_Wye, Data);

      Assert (abs (Data.Total_Current - 10.0) < 0.01, "Current should be ~10A");
      Assert (abs (Data.Total_Voltage - 230.0) < 0.1, "Voltage should be ~230V");
      Assert (abs (Data.Frequency - 50.0) < 0.01, "Frequency should be ~50Hz");
      Assert (abs (Data.Total_Power - 2300.0) < 0.1, "Power should be 2300W");
      Assert (Data.Meter_Kind = Meter_3P_Wye, "Meter type should be Wye");
   end Test_Meter_Totals;

   ---------------------------------------------------------------------------
   --  Storage package tests
   ---------------------------------------------------------------------------

   procedure Test_Storage_Status_Decode (T : in Out Test_Case'Class);
   procedure Test_Storage_Status_Decode (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : Protocol.PDU_Buffer := [others => 0];
      SOC, SOH : Float;
      Stat : Storage.Storage_Status;
      Result : Ada_Modbus.Status;
   begin
      --  Build response: FC=03, ByteCount=12, 6 registers
      Buffer (0) := 16#03#;
      Buffer (1) := 12;
      --  SOC = 7500 (75.00% with SF=-2) at offset 2
      Buffer (2) := 16#1D#; Buffer (3) := 16#4C#;  -- 7500
      --  SOC_SF = -2 at offset 4
      Buffer (4) := 16#FF#; Buffer (5) := 16#FE#;  -- -2
      --  SOH = 9800 (98.00% with SF=-2) at offset 6
      Buffer (6) := 16#26#; Buffer (7) := 16#48#;  -- 9800
      --  SOH_SF = -2 at offset 8
      Buffer (8) := 16#FF#; Buffer (9) := 16#FE#;  -- -2
      --  Status = 4 (Charging) at offset 10
      Buffer (10) := 16#00#; Buffer (11) := 16#04#;
      --  Charge status at offset 12
      Buffer (12) := 16#00#; Buffer (13) := 16#00#;

      Decode_Status_Response (Buffer, 14, SOC, SOH, Stat, Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (abs (SOC - 75.0) < 0.01, "SOC should be ~75%");
      Assert (abs (SOH - 98.0) < 0.01, "SOH should be ~98%");
      Assert (Stat = Charging, "Status should be Charging");
   end Test_Storage_Status_Decode;

   procedure Test_Storage_Capacity_Decode (T : in Out Test_Case'Class);
   procedure Test_Storage_Capacity_Decode (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : Protocol.PDU_Buffer := [others => 0];
      Max_Cha, Max_Dis, Stored, Cap : Float;
      Result : Ada_Modbus.Status;
   begin
      --  Build response: FC=03, ByteCount=16, 8 registers
      Buffer (0) := 16#03#;
      Buffer (1) := 16;
      --  Max charge = 5000W, SF = 0 at offset 2-5
      Buffer (2) := 16#13#; Buffer (3) := 16#88#;  -- 5000
      Buffer (4) := 16#00#; Buffer (5) := 16#00#;  -- SF=0
      --  Max discharge = 5000W, SF = 0 at offset 6-9
      Buffer (6) := 16#13#; Buffer (7) := 16#88#;  -- 5000
      Buffer (8) := 16#00#; Buffer (9) := 16#00#;  -- SF=0
      --  Stored = 7500Wh, SF = 0 at offset 10-13
      Buffer (10) := 16#1D#; Buffer (11) := 16#4C#;  -- 7500
      Buffer (12) := 16#00#; Buffer (13) := 16#00#;  -- SF=0
      --  Capacity = 10000Wh, SF = 0 at offset 14-17
      Buffer (14) := 16#27#; Buffer (15) := 16#10#;  -- 10000
      Buffer (16) := 16#00#; Buffer (17) := 16#00#;  -- SF=0

      Decode_Capacity_Response (Buffer, 18, Max_Cha, Max_Dis, Stored, Cap, Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (abs (Max_Cha - 5000.0) < 0.1, "Max charge should be 5000W");
      Assert (abs (Max_Dis - 5000.0) < 0.1, "Max discharge should be 5000W");
      Assert (abs (Stored - 7500.0) < 0.1, "Stored should be 7500Wh");
      Assert (abs (Cap - 10000.0) < 0.1, "Capacity should be 10000Wh");
   end Test_Storage_Capacity_Decode;

   ---------------------------------------------------------------------------
   --  Nameplate package tests
   ---------------------------------------------------------------------------

   procedure Test_Nameplate_Decode (T : in Out Test_Case'Class);
   procedure Test_Nameplate_Decode (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Regs : Register_Array (0 .. 26) := [others => 0];
      SF   : Nameplate_Scale_Factors;
      Data : Nameplate_Data;
   begin
      --  All offsets use (Reg_X - 2) in the implementation
      --  DER Type = 4 (PV)
      Regs (Nameplate.Reg_DERTyp - 2) := 4;
      --  WRtg = 10000W, SF = 0
      Regs (Nameplate.Reg_WRtg - 2) := 10000;
      Regs (Nameplate.Reg_WRtg_SF - 2) := 0;
      --  VARtg = 11000VA, SF = 0
      Regs (Nameplate.Reg_VARtg - 2) := 11000;
      Regs (Nameplate.Reg_VARtg_SF - 2) := 0;
      --  Other scale factors = 0
      Regs (Nameplate.Reg_VArRtg_SF - 2) := 0;
      Regs (Nameplate.Reg_ARtg_SF - 2) := 0;
      Regs (Nameplate.Reg_PFRtg_SF - 2) := 0;
      Regs (Nameplate.Reg_WHRtg_SF - 2) := 0;
      Regs (Nameplate.Reg_AhrRtg_SF - 2) := 0;
      Regs (Nameplate.Reg_MaxChaRte_SF - 2) := 0;
      Regs (Nameplate.Reg_MaxDisChaRte_SF - 2) := 0;
      --  Max current = 45A
      Regs (Nameplate.Reg_ARtg - 2) := 450;
      Regs (Nameplate.Reg_ARtg_SF - 2) := 65535;  -- SF = -1

      Decode_Nameplate_Scale_Factors (Regs, SF);
      Decode_Nameplate (Regs, SF, Data);

      Assert (Data.DER_Kind = DER_PV, "DER type should be PV");
      Assert (abs (Data.Max_Power_W - 10000.0) < 0.1, "Max power should be 10kW");
      Assert (abs (Data.Max_VA - 11000.0) < 0.1, "Max VA should be 11kVA");
      Assert (abs (Data.Max_Current_A - 45.0) < 0.1, "Max current should be 45A");
   end Test_Nameplate_Decode;

   ---------------------------------------------------------------------------
   --  Settings package tests
   ---------------------------------------------------------------------------

   procedure Test_Settings_Decode (T : in Out Test_Case'Class);
   procedure Test_Settings_Decode (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Regs : Register_Array (0 .. 31) := [others => 0];
      SF   : Settings_Scale_Factors;
      Data : Settings_Data;
   begin
      --  All offsets use (Reg_X - 2) in the implementation
      --  WMax = 8000W, SF = 0
      Regs (Settings.Reg_WMax - 2) := 8000;
      Regs (Settings.Reg_WMax_SF - 2) := 0;
      --  VRef = 230V, SF = 0
      Regs (Settings.Reg_VRef - 2) := 230;
      Regs (Settings.Reg_VRef_SF - 2) := 0;
      --  VMax = 253V, VMin = 207V, VMinMax_SF = 0
      Regs (Settings.Reg_VMax - 2) := 253;
      Regs (Settings.Reg_VMin - 2) := 207;
      Regs (Settings.Reg_VMinMax_SF - 2) := 0;
      --  Nominal Hz = 5000 (50.00Hz with SF=-2)
      Regs (Settings.Reg_ECPNomHz - 2) := 5000;
      Regs (Settings.Reg_ECPNomHz_SF - 2) := 65534;  -- -2
      --  Connected phases = 7 (ABC)
      Regs (Settings.Reg_ConnPh - 2) := 7;
      --  Other SFs = 0
      Regs (Settings.Reg_VRefOfs_SF - 2) := 0;
      Regs (Settings.Reg_VAMax_SF - 2) := 0;
      Regs (Settings.Reg_VArMax_SF - 2) := 0;
      Regs (Settings.Reg_WGra_SF - 2) := 0;
      Regs (Settings.Reg_PFMin_SF - 2) := 0;

      Decode_Settings_Scale_Factors (Regs, SF);
      Decode_Settings (Regs, SF, Data);

      Assert (abs (Data.Max_Power_W - 8000.0) < 0.1, "Max power should be 8kW");
      Assert (abs (Data.Ref_Voltage_V - 230.0) < 0.1, "Ref voltage should be 230V");
      Assert (abs (Data.Max_Voltage_V - 253.0) < 0.1, "Max voltage should be 253V");
      Assert (abs (Data.Min_Voltage_V - 207.0) < 0.1, "Min voltage should be 207V");
      Assert (abs (Data.Nominal_Hz - 50.0) < 0.01, "Nominal Hz should be 50");
      Assert (Data.Conn_Phase = Phase_ABC, "Connected phases should be ABC");
   end Test_Settings_Decode;

   ---------------------------------------------------------------------------
   --  DER package tests
   ---------------------------------------------------------------------------

   procedure Test_DER_Power_Limit_Encode (T : in Out Test_Case'Class);
   procedure Test_DER_Power_Limit_Encode (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Regs : Register_Array (0 .. 3);
   begin
      Encode_Power_Limit (Limit_Pct => 60.0, Enabled => True, Regs => Regs);

      --  Enabled = 1
      Assert (Regs (0) = 1, "Enable register should be 1");
      --  60% = 6000 (assuming SF=-2 internally, or 60 if SF=0)
      Assert (Regs (1) > 0, "Limit should be set");
   end Test_DER_Power_Limit_Encode;

   procedure Test_DER_Status_Decode (T : in Out Test_Case'Class);
   procedure Test_DER_Status_Decode (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Regs : Register_Array (0 .. 119) := [others => 0];
      SF   : DER_Status_Scale_Factors;
      Stat : DER_Status;
   begin
      --  All offsets use (Reg_X - 2) in the implementation
      --  Operating state = 4 (Running)
      Regs (DER.Reg_701_St - 2) := 4;
      --  Connection state = 2 (Connected)
      Regs (DER.Reg_701_ConnSt - 2) := 2;
      --  Active power = 5000W (SF = 0)
      Regs (DER.Reg_701_W - 2) := 5000;
      Regs (DER.Reg_701_W_SF - 2) := 0;
      --  Frequency = 5000 (32-bit: high word = 0, low word = 5000) with SF=-2
      Regs (DER.Reg_701_Hz - 2) := 0;      -- High word
      Regs (DER.Reg_701_Hz - 1) := 5000;   -- Low word
      Regs (DER.Reg_701_Hz_SF - 2) := 65534;  -- -2
      --  Other scale factors = 0
      Regs (DER.Reg_701_VA_SF - 2) := 0;
      Regs (DER.Reg_701_Var_SF - 2) := 0;
      Regs (DER.Reg_701_PF_SF - 2) := 0;
      --  Throttle = 0 (not throttled)
      Regs (DER.Reg_701_ThrotPct - 2) := 0;

      Decode_Status_Scale_Factors (Regs, SF);
      Decode_DER_Status (Regs, SF, Stat);

      Assert (Stat.Operating_State = State_Running, "State should be Running");
      Assert (Stat.Conn_State = Connected, "Connection should be Connected");
      Assert (abs (Stat.Active_Power_W - 5000.0) < 0.1, "Power should be 5000W");
      Assert (abs (Stat.Frequency_Hz - 50.0) < 0.01, "Frequency should be 50Hz");
      Assert (not Stat.Is_Throttled, "Should not be throttled");
   end Test_DER_Status_Decode;

   ---------------------------------------------------------------------------
   --  Battery package tests
   ---------------------------------------------------------------------------

   procedure Test_Battery_Decode (T : in Out Test_Case'Class);
   procedure Test_Battery_Decode (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Regs : Register_Array (0 .. 63) := [others => 0];
      SF   : Battery_Scale_Factors;
      Data : Battery_Data;
   begin
      --  All offsets use (Reg_X - 2) in the implementation
      --  Capacity: 100Ah, 5000Wh
      Regs (Battery.Reg_AHRtg - 2) := 100;
      Regs (Battery.Reg_AHRtg_SF - 2) := 0;
      Regs (Battery.Reg_WHRtg - 2) := 5000;
      Regs (Battery.Reg_WHRtg_SF - 2) := 0;
      --  Max charge/discharge = 2500W
      Regs (Battery.Reg_WChaRteMax - 2) := 2500;
      Regs (Battery.Reg_WDisChaRteMax - 2) := 2500;
      Regs (Battery.Reg_WChaDisChaMax_SF - 2) := 0;
      --  SOC = 8000 (80.00%, SF=-2)
      Regs (Battery.Reg_SoC - 2) := 8000;
      Regs (Battery.Reg_SoC_SF - 2) := 65534;  -- -2
      --  SOH = 9900 (99.00%, SF=-2)
      Regs (Battery.Reg_SoH - 2) := 9900;
      Regs (Battery.Reg_SoH_SF - 2) := 65534;  -- -2
      --  DOD = 2000 (20.00%, SF=-2)
      Regs (Battery.Reg_DoD - 2) := 2000;
      Regs (Battery.Reg_DoD_SF - 2) := 65534;  -- -2
      --  Cycle count (uint32) = 500: high word at Reg_NCyc, low word at Reg_NCyc+1
      Regs (Battery.Reg_NCyc - 2) := 0;
      Regs (Battery.Reg_NCyc - 1) := 500;
      --  Voltage = 512 (51.2V with SF=-1)
      Regs (Battery.Reg_V - 2) := 512;
      Regs (Battery.Reg_V_SF - 2) := 65535;  -- -1
      --  Battery type = 4 (LiIon)
      Regs (Battery.Reg_Typ - 2) := 4;
      --  Battery state = 7 (Charging)
      Regs (Battery.Reg_State - 2) := 7;
      --  Charge state = 4 (Charging)
      Regs (Battery.Reg_ChaSt - 2) := 4;
      --  Cell voltages
      Regs (Battery.Reg_CellVMax - 2) := 420;
      Regs (Battery.Reg_CellVMin - 2) := 380;
      Regs (Battery.Reg_CellVAvg - 2) := 400;
      Regs (Battery.Reg_CellV_SF - 2) := 65534;  -- -2 (4.20V, 3.80V, 4.00V)
      --  Current and power
      Regs (Battery.Reg_A - 2) := 100;  -- 10.0A with SF=-1
      Regs (Battery.Reg_A_SF - 2) := 65535;  -- -1
      Regs (Battery.Reg_W - 2) := 500;
      Regs (Battery.Reg_W_SF - 2) := 0;

      Decode_Battery_Scale_Factors (Regs, SF);
      Decode_Battery_Data (Regs, SF, Data);

      Assert (abs (Data.Capacity_Ah - 100.0) < 0.1, "Capacity should be 100Ah");
      Assert (abs (Data.Capacity_Wh - 5000.0) < 0.1, "Capacity should be 5000Wh");
      Assert (abs (Data.SOC_Percent - 80.0) < 0.01, "SOC should be 80%");
      Assert (abs (Data.SOH_Percent - 99.0) < 0.01, "SOH should be 99%");
      Assert (abs (Data.DOD_Percent - 20.0) < 0.01, "DOD should be 20%");
      Assert (abs (Data.Voltage_V - 51.2) < 0.1, "Voltage should be 51.2V");
      Assert (Data.Bat_Type = Battery_LiIon, "Type should be LiIon");
      Assert (Data.Bat_State = Battery.State_Charging, "State should be Charging");
      Assert (abs (Data.Cell_V_Max - 4.2) < 0.01, "Max cell should be 4.2V");
      Assert (abs (Data.Cell_V_Min - 3.8) < 0.01, "Min cell should be 3.8V");
   end Test_Battery_Decode;

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
      --  Additional tests for signed values and Inverter model
      Registration.Register_Routine (T, Test_Apply_Scale_Signed'Access,
                          "Apply_Scale_Signed (signed int16)");
      Registration.Register_Routine (T, Test_Inverter_AC_Decode'Access,
                          "Inverter AC Measurements Decode");
      Registration.Register_Routine (T, Test_Inverter_DC_Decode'Access,
                          "Inverter DC Measurements Decode");
      Registration.Register_Routine (T, Test_Inverter_Energy_Decode'Access,
                          "Inverter Energy Decode (32-bit)");
      Registration.Register_Routine (T, Test_Inverter_State_Decode'Access,
                          "Inverter State Decode");
      Registration.Register_Routine (T, Test_Common_Model_Requests'Access,
                          "Common Model Request Encoding");
      Registration.Register_Routine (T, Test_To_Scale_Factor'Access,
                          "To_Scale_Factor conversion");
      Registration.Register_Routine (T, Test_To_Signed_16'Access,
                          "To_Signed_16 conversion");
      Registration.Register_Routine (T, Test_Is_Implemented'Access,
                          "Is_Implemented checks");
      --  Meter package tests
      Registration.Register_Routine (T, Test_Meter_To_Type'Access,
                          "Meter To_Meter_Type conversion");
      Registration.Register_Routine (T, Test_Meter_Scale_Factors'Access,
                          "Meter scale factors decode");
      Registration.Register_Routine (T, Test_Meter_Totals'Access,
                          "Meter totals decode");
      --  Storage package tests
      Registration.Register_Routine (T, Test_Storage_Status_Decode'Access,
                          "Storage status decode");
      Registration.Register_Routine (T, Test_Storage_Capacity_Decode'Access,
                          "Storage capacity decode");
      --  Nameplate package tests
      Registration.Register_Routine (T, Test_Nameplate_Decode'Access,
                          "Nameplate decode");
      --  Settings package tests
      Registration.Register_Routine (T, Test_Settings_Decode'Access,
                          "Settings decode");
      --  DER package tests
      Registration.Register_Routine (T, Test_DER_Power_Limit_Encode'Access,
                          "DER power limit encode");
      Registration.Register_Routine (T, Test_DER_Status_Decode'Access,
                          "DER status decode");
      --  Battery package tests
      Registration.Register_Routine (T, Test_Battery_Decode'Access,
                          "Battery decode");
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
