--  Kostal_Reader - Example for reading Kostal inverter via SunSpec/Modbus TCP
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Tested with: PLENTICORE plus, PIKO IQ, PIKO CI
--
--  Kostal Configuration:
--    Port: 1502 (Kostal default, not standard 502)
--    Unit ID: 71 (default)
--    Enable: Settings -> Modbus/SunSpec (TCP) -> Activate Modbus
--
--  Usage: kostal_reader <ip-address> [port] [unit-id]
--  Example: kostal_reader 192.168.1.50
--           kostal_reader 192.168.1.50 1502 71

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Command_Line;
with Ada.Calendar;
with Ada.Exceptions;
with Interfaces; use Interfaces;
use type Interfaces.Integer_16;

with Ada_Modbus;
with Ada_Modbus.Master;
with Ada_Modbus.Transport.TCP;
with Ada_Modbus.Energy.SunSpec;
with Ada_Modbus.Energy.SunSpec.Common;
with Ada_Modbus.Energy.SunSpec.Inverter;

procedure Kostal_Reader is

   use Ada_Modbus;
   use Ada_Modbus.Transport.TCP;
   use Ada_Modbus.Energy.SunSpec;

   --  Kostal default settings
   Default_Port    : constant := 1502;
   Default_Unit_Id : constant := 71;
   SunSpec_Base    : constant Register_Address := 40000;

   --  Connection and access type for transport context (required for limited type)
   type Connection_Access is access all TCP_Connection;
   Connection : aliased TCP_Connection;
   Conn_Ptr   : constant Connection_Access := Connection'Access;

   --  Transport callbacks for Master generic
   function Send_Data
     (Ctx  : in out Connection_Access;
      Data : Byte_Array) return Natural is
   begin
      return Send (Ctx.all, Data);
   end Send_Data;

   function Receive_Data
     (Ctx        : in out Connection_Access;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural is
   begin
      return Receive (Ctx.all, Buffer, Max_Length, Timeout_Ms);
   end Receive_Data;

   function Get_Tick return Unsigned_32 is
      use Ada.Calendar;
      Now     : constant Time := Clock;
      --  Use seconds since midnight to avoid overflow
      Seconds : constant Day_Duration := Ada.Calendar.Seconds (Now);
   begin
      return Unsigned_32 (Seconds * 1000.0) mod Unsigned_32'Last;
   end Get_Tick;

   --  Instantiate master
   package Modbus_Master is new Ada_Modbus.Master
     (Transport_Context => Connection_Access,
      Send              => Send_Data,
      Receive           => Receive_Data,
      Get_Tick_Ms       => Get_Tick);

   Ctx : Modbus_Master.Master_Context;

   --  Helper: Read registers with error handling
   function Read_Registers
     (Slave    : Unit_Id;
      Address  : Register_Address;
      Quantity : Natural;
      Values   : out Register_Array) return Boolean
   is
      Result : Status;
   begin
      Result := Modbus_Master.Read_Holding_Registers
        (Ctx, Slave, Address, Register_Count (Quantity), Values);
      if Result /= Success then
         Put_Line ("  Error reading registers at " & Address'Image &
                   ": " & Result'Image);
         return False;
      end if;
      return True;
   end Read_Registers;

   --  Check SunSpec identifier
   function Check_SunSpec (Slave : Unit_Id) return Boolean is
      Values : Register_Array (0 .. 1);
   begin
      if not Read_Registers (Slave, SunSpec_Base, 2, Values) then
         return False;
      end if;
      return Values (0) = SunS_ID_High and Values (1) = SunS_ID_Low;
   end Check_SunSpec;

   --  Read and display Common Model (Model 1)
   procedure Read_Common_Model
     (Slave       : Unit_Id;
      Model_Start : Register_Address)
   is
      use Ada_Modbus.Energy.SunSpec.Common;
      Str_Regs : Register_Array (0 .. 15);
      Str_Val  : SunSpec_String;
      Str_Len  : Natural;
   begin
      Put_Line ("--- Device Information (Model 1) ---");

      --  Read Manufacturer (16 registers at offset 2)
      if Read_Registers (Slave, Model_Start + 2, 16, Str_Regs) then
         Decode_String (Str_Regs, Str_Val, Str_Len);
         Put_Line ("  Manufacturer: " & Str_Val (1 .. Str_Len));
      end if;

      --  Read Model (16 registers at offset 18)
      if Read_Registers (Slave, Model_Start + 18, 16, Str_Regs) then
         Decode_String (Str_Regs, Str_Val, Str_Len);
         Put_Line ("  Model:        " & Str_Val (1 .. Str_Len));
      end if;

      --  Read Serial (16 registers at offset 50)
      if Read_Registers (Slave, Model_Start + 50, 16, Str_Regs) then
         Decode_String (Str_Regs, Str_Val, Str_Len);
         Put_Line ("  Serial:       " & Str_Val (1 .. Str_Len));
      end if;

      --  Read Version (8 registers at offset 42)
      declare
         Ver_Regs : Register_Array (0 .. 7);
         Ver_Str  : SunSpec_String;
      begin
         if Read_Registers (Slave, Model_Start + 42, 8, Ver_Regs) then
            Decode_String (Ver_Regs, Ver_Str, Str_Len);
            Put_Line ("  Version:      " & Ver_Str (1 .. Str_Len));
         end if;
      end;
   end Read_Common_Model;

   --  Format float without E notation (e.g., "1234.56" instead of "1.23456E+03")
   function Fmt (Value : Float; Decimals : Natural := 2) return String is
      Result : String (1 .. 20);
   begin
      Ada.Float_Text_IO.Put (Result, Value, Aft => Decimals, Exp => 0);
      --  Trim leading spaces
      for I in Result'Range loop
         if Result (I) /= ' ' then
            return Result (I .. Result'Last);
         end if;
      end loop;
      return Result;
   end Fmt;

   --  Convert unsigned register to signed scale factor
   --  SunSpec scale factors are signed 16-bit stored as unsigned
   function To_Scale_Factor (Reg : Register_Value) return Scale_Factor is
      Raw : Integer;
   begin
      --  Convert unsigned to signed (two's complement)
      if Reg > 32767 then
         Raw := Integer (Reg) - 65536;
      else
         Raw := Integer (Reg);
      end if;

      if Raw in -10 .. 10 then
         return Scale_Factor (Raw);
      else
         --  Invalid SF, use 0 (no scaling)
         return 0;
      end if;
   end To_Scale_Factor;

   --  Read and display Inverter Model (Model 101/102/103)
   procedure Read_Inverter_Model
     (Slave       : Unit_Id;
      Model_Start : Register_Address;
      Model_Id    : Natural)
   is
      use Ada_Modbus.Energy.SunSpec.Inverter;
      AC_Regs   : Register_Array (0 .. 15);  --  Registers 2-17
      DC_Regs   : Register_Array (0 .. 5);   --  Registers 27-32
      Energy_Regs : Register_Array (0 .. 2); --  Registers 24-26
      State_Regs  : Register_Array (0 .. 1); --  Registers 38-39
      Temp_Regs   : Register_Array (0 .. 4); --  Registers 33-37
   begin
      New_Line;
      Put_Line ("--- Inverter Data (Model " & Model_Id'Image & ") ---");

      --  Read AC measurements (regs 2-17)
      if Read_Registers (Slave, Model_Start + 2, 16, AC_Regs) then
         declare
            Current_SF : constant Scale_Factor := To_Scale_Factor (AC_Regs (4));
            Voltage_SF : constant Scale_Factor := To_Scale_Factor (AC_Regs (11));
            Power_SF   : constant Scale_Factor := To_Scale_Factor (AC_Regs (13));
            Freq_SF    : constant Scale_Factor := To_Scale_Factor (AC_Regs (15));
         begin
            Put_Line ("  AC Current:   " &
                      Fmt (Apply_Scale (AC_Regs (0), Current_SF)) & " A");
            Put_Line ("  AC Voltage:   " &
                      Fmt (Apply_Scale (AC_Regs (8), Voltage_SF), 1) & " V");
            Put_Line ("  AC Power:     " &
                      Fmt (Apply_Scale (AC_Regs (12), Power_SF), 0) & " W");
            Put_Line ("  AC Frequency: " &
                      Fmt (Apply_Scale (AC_Regs (14), Freq_SF)) & " Hz");
         end;
      end if;

      --  Read DC measurements (regs 27-32)
      --  Note: 0xFFFF = Not Implemented in SunSpec
      if Read_Registers (Slave, Model_Start + 27, 6, DC_Regs) then
         declare
            DC_A_SF : constant Scale_Factor := To_Scale_Factor (DC_Regs (1));
            DC_V_SF : constant Scale_Factor := To_Scale_Factor (DC_Regs (3));
            DC_W_SF : constant Scale_Factor := To_Scale_Factor (DC_Regs (5));
         begin
            if DC_Regs (0) /= 16#FFFF# then
               Put_Line ("  DC Current:   " &
                         Fmt (Apply_Scale (DC_Regs (0), DC_A_SF)) & " A");
            end if;
            if DC_Regs (2) /= 16#FFFF# then
               Put_Line ("  DC Voltage:   " &
                         Fmt (Apply_Scale (DC_Regs (2), DC_V_SF), 1) & " V");
            end if;
            if DC_Regs (4) /= 16#FFFF# then
               Put_Line ("  DC Power:     " &
                         Fmt (Apply_Scale (DC_Regs (4), DC_W_SF), 0) & " W");
            end if;
         end;
      end if;

      --  Read Energy (regs 24-26: 32-bit value + SF)
      if Read_Registers (Slave, Model_Start + 24, 3, Energy_Regs) then
         declare
            Energy_Raw : constant Unsigned_32 :=
              Unsigned_32 (Energy_Regs (0)) * 65536 + Unsigned_32 (Energy_Regs (1));
            Energy_SF  : constant Scale_Factor := To_Scale_Factor (Energy_Regs (2));
            Energy_Wh  : constant Float :=
              Float (Energy_Raw) * Scale_Multipliers (Energy_SF);
         begin
            Put_Line ("  Total Energy: " & Fmt (Energy_Wh / 1000.0, 1) & " kWh");
         end;
      end if;

      --  Read Temperature (regs 33-37)
      --  Note: Temperature is signed int16 in SunSpec
      if Read_Registers (Slave, Model_Start + 33, 5, Temp_Regs) then
         declare
            Temp_SF : constant Scale_Factor := To_Scale_Factor (Temp_Regs (4));
         begin
            Put_Line ("  Cabinet Temp: " &
                      Fmt (Apply_Scale_Signed (Temp_Regs (0), Temp_SF), 1) & " C");
         end;
      end if;

      --  Read Operating State (regs 38-39)
      if Read_Registers (Slave, Model_Start + 38, 2, State_Regs) then
         declare
            State_Code : constant Natural := Natural (State_Regs (0));
            State_Name : constant String :=
              (case State_Code is
                  when 1 => "Off",
                  when 2 => "Sleeping",
                  when 3 => "Starting",
                  when 4 => "Running (MPPT)",
                  when 5 => "Throttled",
                  when 6 => "Shutting Down",
                  when 7 => "Fault",
                  when 8 => "Standby",
                  when others => "Unknown (" & State_Code'Image & ")");
         begin
            Put_Line ("  State:        " & State_Name);
         end;
      end if;
   end Read_Inverter_Model;

   --  Read and display MPPT Model (Model 160)
   --  This provides per-string DC measurements
   procedure Read_MPPT_Model
     (Slave       : Unit_Id;
      Model_Start : Register_Address;
      Model_Len   : Natural)
   is
      --  Model 160 structure:
      --  Offset 2: DCA_SF (int16) - Current scale factor
      --  Offset 3: DCV_SF (int16) - Voltage scale factor
      --  Offset 4: DCW_SF (int16) - Power scale factor
      --  Offset 5: DCWH_SF (int16) - Energy scale factor
      --  Offset 6: Evt (uint32) - Global events
      --  Offset 8: N (uint16) - Number of modules
      --  Offset 9: TmsPer (uint16) - Timestamp period
      --  Offset 10+: Module data (20 registers each)
      --    +0: ID (uint16)
      --    +1: IDStr (string, 8 registers)
      --    +9: DCA (uint16) - DC Current
      --    +10: DCV (uint16) - DC Voltage
      --    +11: DCW (uint16) - DC Power
      --    +12: DCWH (uint32) - Lifetime energy
      --    +14: Tms (uint32) - Timestamp
      --    +16: Tmp (int16) - Temperature
      --    +17: DCSt (uint16) - Operating state
      --    +18: DCEvt (uint32) - Events

      Header_Regs : Register_Array (0 .. 7);  --  Offsets 2-9
      Module_Regs : Register_Array (0 .. 19); --  Per-module data
      Module_Size : constant := 20;
   begin
      New_Line;
      Put_Line ("--- MPPT Data (Model 160) ---");

      --  Read header (scale factors and module count)
      if not Read_Registers (Slave, Model_Start + 2, 8, Header_Regs) then
         return;
      end if;

      declare
         DCA_SF : constant Scale_Factor := To_Scale_Factor (Header_Regs (0));
         DCV_SF : constant Scale_Factor := To_Scale_Factor (Header_Regs (1));
         DCW_SF : constant Scale_Factor := To_Scale_Factor (Header_Regs (2));
         Num_Modules : constant Natural := Natural (Header_Regs (6));
         Module_Base : Register_Address := Model_Start + 10;
      begin
         Put_Line ("  Modules: " & Num_Modules'Image);

         --  Read each module
         for M in 1 .. Num_Modules loop
            exit when Natural (Module_Base - Model_Start) + Module_Size > Model_Len + 2;

            if Read_Registers (Slave, Module_Base, Module_Size, Module_Regs) then
               declare
                  Module_ID : constant Natural := Natural (Module_Regs (0));
                  DC_Current : constant Float :=
                    Apply_Scale (Module_Regs (9), DCA_SF);
                  DC_Voltage : constant Float :=
                    Apply_Scale (Module_Regs (10), DCV_SF);
                  DC_Power : constant Float :=
                    Apply_Scale (Module_Regs (11), DCW_SF);
               begin
                  New_Line;
                  Put_Line ("  String " & Module_ID'Image & ":");

                  if Module_Regs (9) /= 16#FFFF# then
                     Put_Line ("    Current: " & Fmt (DC_Current) & " A");
                  end if;
                  if Module_Regs (10) /= 16#FFFF# then
                     Put_Line ("    Voltage: " & Fmt (DC_Voltage, 1) & " V");
                  end if;
                  if Module_Regs (11) /= 16#FFFF# then
                     Put_Line ("    Power:   " & Fmt (DC_Power, 0) & " W");
                  end if;
               end;
            end if;

            Module_Base := Module_Base + Module_Size;
         end loop;
      end;
   end Read_MPPT_Model;

   --  Main variables
   Host     : String (1 .. 64) := [others => ' '];
   Host_Len : Natural := 0;
   Port     : Natural := Default_Port;
   Unit     : Unit_Id := Default_Unit_Id;
   Result   : Status;

begin
   Put_Line ("=== Kostal Inverter Reader (SunSpec) ===");
   New_Line;

   --  Parse command line
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("Usage: kostal_reader <ip-address> [port] [unit-id]");
      Put_Line ("  Default port: 1502");
      Put_Line ("  Default unit: 71");
      return;
   end if;

   declare
      Arg1 : constant String := Ada.Command_Line.Argument (1);
   begin
      Host_Len := Natural'Min (Arg1'Length, 64);
      Host (1 .. Host_Len) := Arg1 (1 .. Host_Len);
   end;

   if Ada.Command_Line.Argument_Count >= 2 then
      Port := Natural'Value (Ada.Command_Line.Argument (2));
   end if;

   if Ada.Command_Line.Argument_Count >= 3 then
      Unit := Unit_Id'Value (Ada.Command_Line.Argument (3));
   end if;

   Put_Line ("Connecting to " & Host (1 .. Host_Len) &
             ":" & Port'Image & " (Unit " & Unit'Image & ")...");

   --  Connect
   Connect (Connection, Host (1 .. Host_Len), Port, 5.0, Result);
   if Result /= Success then
      Put_Line ("Connection failed: " & Result'Image);
      return;
   end if;
   Put_Line ("Connected.");
   New_Line;

   --  Initialize master
   Modbus_Master.Initialize
     (Ctx,
      (Mode => Modbus_Master.TCP, Default_Slave => Unit, Default_Timeout => 3000),
      Conn_Ptr);

   --  Check SunSpec identifier
   Put ("Checking SunSpec identifier... ");
   if not Check_SunSpec (Unit) then
      Put_Line ("Not found!");
      Put_Line ("This device does not appear to support SunSpec.");
      Put_Line ("Make sure Modbus/SunSpec is enabled in the inverter settings.");
      Disconnect (Connection);
      return;
   end if;
   Put_Line ("OK (SunS found at 40000)");
   New_Line;

   --  Walk through models
   declare
      Offset     : Register_Address := 2;  --  After SunS identifier
      Max_Offset : constant Register_Address := 500;
      Header     : Register_Array (0 .. 1);
   begin
      while Offset < Max_Offset loop
         if not Read_Registers (Unit, SunSpec_Base + Offset, 2, Header) then
            exit;
         end if;

         --  Check for end marker (0xFFFF)
         if Header (0) = 16#FFFF# then
            New_Line;
            Put_Line ("--- End of Model List ---");
            exit;
         end if;

         declare
            Model_Id  : constant Natural := Natural (Header (0));
            Model_Len : constant Natural := Natural (Header (1));
            Model_Start : constant Register_Address := SunSpec_Base + Offset;
         begin
            case Model_Id is
               when 1 =>
                  --  Common Model
                  Read_Common_Model (Unit, Model_Start);

               when 101 | 102 | 103 =>
                  --  Inverter Model
                  Read_Inverter_Model (Unit, Model_Start, Model_Id);

               when 124 =>
                  --  Storage Model (if battery connected)
                  New_Line;
                  Put_Line ("--- Battery Storage (Model 124) found ---");
                  Put_Line ("  (Storage readout not implemented in this example)");

               when 160 =>
                  --  MPPT Model (per-string DC data)
                  Read_MPPT_Model (Unit, Model_Start, Model_Len);

               when others =>
                  --  Skip unknown models
                  null;
            end case;

            --  Move to next model (header + data)
            Offset := Offset + 2 + Register_Address (Model_Len);
         end;
      end loop;
   end;

   New_Line;
   Put_Line ("Done.");
   Disconnect (Connection);

exception
   when E : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
      Disconnect (Connection);
end Kostal_Reader;
