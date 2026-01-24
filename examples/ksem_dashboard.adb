--  KSEM_Dashboard - Live Dashboard for Kostal Smart Energy Meter
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Terminal-based live dashboard for KSEM energy meters.
--  Updates every second with color-coded values.
--
--  Usage: ksem_dashboard <ip-address> [port] [unit-id]

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Exceptions;
with Interfaces; use Interfaces;

with Ada_Modbus;
with Ada_Modbus.Master;
with Ada_Modbus.Transport.TCP;
with Ada_Modbus.Energy.SunSpec;
with Ada_Modbus.Energy.SunSpec.Common;
with Ada_Modbus.Energy.SunSpec.Meter;

with Terminal_Dashboard; use Terminal_Dashboard;

procedure KSEM_Dashboard is

   use Ada_Modbus;
   use Ada_Modbus.Transport.TCP;
   use Ada_Modbus.Energy.SunSpec;
   use Ada_Modbus.Energy.SunSpec.Meter;

   --  Dashboard layout
   Width      : constant := 70;
   Height     : constant := 22;
   Left_Col   : constant := 3;
   Right_Col  : constant := 38;
   Val_Offset : constant := 12;

   --  KSEM defaults
   Default_Port    : constant := 502;
   Default_Unit_Id : constant := 1;
   SunSpec_Base    : constant Register_Address := Default_Base_Address;

   --  Connection
   type Connection_Access is access all TCP_Connection;
   Connection : aliased TCP_Connection;
   Conn_Ptr   : constant Connection_Access := Connection'Access;

   function Send_Data
     (Ctx  : in Out Connection_Access;
      Data : Byte_Array) return Natural is
   begin
      return Send (Ctx.all, Data);
   end Send_Data;

   function Receive_Data
     (Ctx        : in Out Connection_Access;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural is
   begin
      return Receive (Ctx.all, Buffer, Max_Length, Timeout_Ms);
   end Receive_Data;

   function Get_Tick return Unsigned_32 is
      use Ada.Calendar;
      Seconds : constant Day_Duration := Ada.Calendar.Seconds (Clock);
   begin
      return Unsigned_32 (Seconds * 1000.0) mod Unsigned_32'Last;
   end Get_Tick;

   package Modbus_Master is new Ada_Modbus.Master
     (Transport_Context => Connection_Access,
      Send              => Send_Data,
      Receive           => Receive_Data,
      Get_Tick_Ms       => Get_Tick);

   Ctx : Modbus_Master.Master_Context;

   --  Cached info
   Device_Model  : String (1 .. 32) := (others => ' ');
   Model_Len     : Natural := 0;
   Meter_Addr    : Register_Address := 0;
   Meter_Model   : Natural := 0;

   --  Current values
   Data : Meter_Data;
   SF   : Meter_Scale_Factors;

   --  Read helper
   function Read_Regs
     (Slave    : Unit_Id;
      Address  : Register_Address;
      Quantity : Natural;
      Values   : out Register_Array) return Boolean
   is
      Result : Status;
   begin
      Result := Modbus_Master.Read_Holding_Registers
        (Ctx, Slave, Address, Register_Count (Quantity), Values);
      return Result = Success;
   end Read_Regs;

   --  Power direction indicator
   function Power_Arrow (Watts : Float) return String is
   begin
      if Watts > 10.0 then
         return ">>> ";  --  Import/Consumption
      elsif Watts < -10.0 then
         return "<<< ";  --  Export/Production
      else
         return "    ";
      end if;
   end Power_Arrow;

   --  Draw static layout
   procedure Draw_Layout (Host : String; Port : Natural; Unit : Unit_Id) is
   begin
      Clear_Screen;
      Hide_Cursor;

      Draw_Box (1, 1, Width, Height, " KOSTAL SMART ENERGY METER ");

      --  Header
      Print (2, Left_Col, Host & ":" & Port'Image & " (Unit" & Unit'Image & ")",
             FG => Cyan);
      if Model_Len > 0 then
         Print (3, Left_Col, Device_Model (1 .. Model_Len), FG => White, S => Bold);
      end if;

      Draw_HLine (4, 2, Width - 2);

      --  Power section
      Print (5, Left_Col, "POWER", FG => Bright_Cyan, S => Bold);
      Print (6, Left_Col, "Total:");
      Print (7, Left_Col, "L1:");
      Print (8, Left_Col, "L2:");
      Print (9, Left_Col, "L3:");

      Print (5, Right_Col, "VOLTAGE (L-N)", FG => Bright_Cyan, S => Bold);
      Print (6, Right_Col, "Average:");
      Print (7, Right_Col, "L1:");
      Print (8, Right_Col, "L2:");
      Print (9, Right_Col, "L3:");

      Draw_HLine (10, 2, Width - 2);

      --  Current section
      Print (11, Left_Col, "CURRENT", FG => Bright_Cyan, S => Bold);
      Print (12, Left_Col, "Total:");
      Print (13, Left_Col, "L1:");
      Print (14, Left_Col, "L2:");
      Print (15, Left_Col, "L3:");

      Print (11, Right_Col, "FREQUENCY / PF", FG => Bright_Cyan, S => Bold);
      Print (12, Right_Col, "Frequency:");
      Print (13, Right_Col, "Power Factor:");

      Draw_HLine (16, 2, Width - 2);

      --  Energy section
      Print (17, Left_Col, "ENERGY", FG => Bright_Cyan, S => Bold);
      Print (18, Left_Col, "Imported:");
      Print (19, Left_Col, "Exported:");

      Print (17, Right_Col, "REACTIVE / APPARENT", FG => Bright_Cyan, S => Bold);
      Print (18, Right_Col, "Reactive:");
      Print (19, Right_Col, "Apparent:");

      Draw_HLine (20, 2, Width - 2);
      Print (21, Left_Col, "Press Ctrl+C to exit", FG => Bright_Black);
   end Draw_Layout;

   --  Update values from meter
   procedure Update_Values (Unit : Unit_Id) is
      Meter_Regs : Register_Array (0 .. 54);
      M_Type     : constant Meter_Type := To_Meter_Type (Model_ID (Meter_Model));
   begin
      if Meter_Addr = 0 then
         return;
      end if;

      if Read_Regs (Unit, Meter_Addr + 2, 55, Meter_Regs) then
         Decode_Meter_Scale_Factors (Meter_Regs, SF);
         Decode_Meter_Totals (Meter_Regs, SF, M_Type, Data);
         if Meter_Model >= 203 then
            Decode_Meter_Phases (Meter_Regs, SF, Data);
         end if;
      end if;
   end Update_Values;

   --  Display current values
   procedure Display_Values is
      use Ada.Calendar.Formatting;
      Now : constant String := Image (Ada.Calendar.Clock);
   begin
      --  Power (negative = export, positive = import)
      Print (6, Left_Col + Val_Offset - 4, Power_Arrow (Data.Total_Power),
             FG => Power_Color (-Data.Total_Power));
      Print (6, Left_Col + Val_Offset, Fmt_Power (Data.Total_Power) & "      ",
             FG => Power_Color (-Data.Total_Power), S => Bold);

      if Meter_Model >= 203 then
         Print (7, Left_Col + Val_Offset, Fmt_Power (Data.Phase_A.Power_W) & "      ",
                FG => Power_Color (-Data.Phase_A.Power_W));
         Print (8, Left_Col + Val_Offset, Fmt_Power (Data.Phase_B.Power_W) & "      ",
                FG => Power_Color (-Data.Phase_B.Power_W));
         Print (9, Left_Col + Val_Offset, Fmt_Power (Data.Phase_C.Power_W) & "      ",
                FG => Power_Color (-Data.Phase_C.Power_W));
      end if;

      --  Voltage
      Print (6, Right_Col + Val_Offset, Fmt (Data.Total_Voltage, 1) & " V    ", FG => White);
      if Meter_Model >= 203 then
         Print (7, Right_Col + Val_Offset, Fmt (Data.Phase_A.Voltage_V, 1) & " V    ", FG => White);
         Print (8, Right_Col + Val_Offset, Fmt (Data.Phase_B.Voltage_V, 1) & " V    ", FG => White);
         Print (9, Right_Col + Val_Offset, Fmt (Data.Phase_C.Voltage_V, 1) & " V    ", FG => White);
      end if;

      --  Current
      Print (12, Left_Col + Val_Offset, Fmt (Data.Total_Current, 2) & " A    ", FG => Cyan);
      if Meter_Model >= 203 then
         Print (13, Left_Col + Val_Offset, Fmt (Data.Phase_A.Current_A, 2) & " A    ", FG => Cyan);
         Print (14, Left_Col + Val_Offset, Fmt (Data.Phase_B.Current_A, 2) & " A    ", FG => Cyan);
         Print (15, Left_Col + Val_Offset, Fmt (Data.Phase_C.Current_A, 2) & " A    ", FG => Cyan);
      end if;

      --  Frequency / PF
      Print (12, Right_Col + Val_Offset, Fmt (Data.Frequency, 2) & " Hz    ", FG => White);
      Print (13, Right_Col + Val_Offset, Fmt (Data.Total_PF, 3) & "       ", FG => White);

      --  Energy
      Print (18, Left_Col + Val_Offset, Fmt_Energy (Data.Total_Imp_Wh) & "      ",
             FG => Yellow);
      Print (19, Left_Col + Val_Offset, Fmt_Energy (Data.Total_Exp_Wh) & "      ",
             FG => Green);

      --  Reactive / Apparent
      Print (18, Right_Col + Val_Offset, Fmt (Data.Total_VAR, 0) & " var    ", FG => Magenta);
      Print (19, Right_Col + Val_Offset, Fmt (Data.Total_VA, 0) & " VA    ", FG => White);

      --  Timestamp
      Print (21, Right_Col, "Updated: " & Now (12 .. 19), FG => Bright_Black);
   end Display_Values;

   --  Discover meter model
   procedure Discover_Meter (Unit : Unit_Id) is
      use Ada_Modbus.Energy.SunSpec.Common;
      Iterator : Model_Iterator;
      Header   : Register_Array (0 .. 1);
      Str_Regs : Register_Array (0 .. 15);
      Str_Val  : SunSpec_String;
   begin
      Init_Model_Iterator (Iterator, SunSpec_Base);

      while Iterator.Is_Valid and Iterator.Current_Offset < 500 loop
         exit when not Read_Regs (Unit, Get_Header_Address (Iterator), 2, Header);
         exit when Header (0) = End_Model_ID;

         declare
            M_ID    : constant Natural := Natural (Header (0));
            M_Len   : constant Natural := Natural (Header (1));
            M_Start : constant Register_Address := Get_Header_Address (Iterator);
         begin
            case M_ID is
               when Model_Common =>
                  if Read_Regs (Unit, M_Start + Reg_Model, 16, Str_Regs) then
                     Decode_String (Str_Regs, Str_Val, Model_Len);
                     Model_Len := Natural'Min (Model_Len, 32);
                     Device_Model (1 .. Model_Len) := Str_Val (1 .. Model_Len);
                  end if;

               when Model_Meter_1P | Model_Meter_SP |
                    Model_Meter_3P_Wye | Model_Meter_3P_Delta =>
                  Meter_Addr  := M_Start;
                  Meter_Model := M_ID;

               when others =>
                  null;
            end case;

            Advance_Model_Iterator (Iterator, Model_Length (M_Len));
         end;
      end loop;
   end Discover_Meter;

   --  Main variables
   Host     : String (1 .. 64) := (others => ' ');
   Host_Len : Natural := 0;
   Port     : Natural := Default_Port;
   Unit     : Unit_Id := Default_Unit_Id;
   Result   : Status;

begin
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("KSEM Dashboard - Live energy meter monitoring");
      Put_Line ("Usage: ksem_dashboard <ip-address> [port] [unit-id]");
      Put_Line ("  Default port: 502");
      Put_Line ("  Default unit: 1");
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

   Put_Line ("Connecting to " & Host (1 .. Host_Len) & ":" & Port'Image & "...");

   Connect (Connection, Host (1 .. Host_Len), Port, 5.0, Result);
   if Result /= Success then
      Put_Line ("Connection failed: " & Result'Image);
      return;
   end if;

   Modbus_Master.Initialize
     (Ctx,
      (Mode => Modbus_Master.TCP, Default_Slave => Unit, Default_Timeout => 2000),
      Conn_Ptr);

   --  Check SunSpec
   declare
      Values : Register_Array (0 .. 1);
   begin
      if not Read_Regs (Unit, SunSpec_Base, 2, Values) or else
         Values (0) /= SunS_ID_High or else Values (1) /= SunS_ID_Low
      then
         Put_Line ("SunSpec not found");
         Disconnect (Connection);
         return;
      end if;
   end;

   Discover_Meter (Unit);

   if Meter_Addr = 0 then
      Put_Line ("No meter model (201-204) found");
      Disconnect (Connection);
      return;
   end if;

   Draw_Layout (Host (1 .. Host_Len), Port, Unit);

   loop
      Update_Values (Unit);
      Display_Values;
      delay 1.0;
   end loop;

exception
   when E : others =>
      Show_Cursor;
      Reset_Style;
      Move_To (Height + 1, 1);
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
      Disconnect (Connection);
end KSEM_Dashboard;
