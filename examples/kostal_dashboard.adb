--  Kostal_Dashboard - Live Dashboard for Kostal Inverters
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Terminal-based live dashboard for Kostal inverters (PLENTICORE, PIKO)
--  Updates every second with color-coded values.
--
--  Usage: kostal_dashboard <ip-address> [port] [unit-id]

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
with Ada_Modbus.Energy.SunSpec.Inverter;
with Ada_Modbus.Energy.SunSpec.Storage;

with Terminal_Dashboard; use Terminal_Dashboard;

procedure Kostal_Dashboard is

   use Ada_Modbus;
   use Ada_Modbus.Transport.TCP;
   use Ada_Modbus.Energy.SunSpec;
   use Ada_Modbus.Energy.SunSpec.Inverter;

   --  Dashboard layout constants
   Width      : constant := 70;
   Height     : constant := 24;
   Left_Col   : constant := 3;
   Right_Col  : constant := 38;
   Val_Offset : constant := 14;

   --  Kostal defaults
   Default_Port    : constant := 1502;
   Default_Unit_Id : constant := 71;
   SunSpec_Base    : constant Register_Address := Default_Base_Address;

   --  Connection
   type Connection_Access is access all TCP_Connection;
   Connection : aliased TCP_Connection;
   Conn_Ptr   : constant Connection_Access := Connection'Access;

   --  Transport callbacks
   function Send_Data
     (Ctx  : in out Connection_Access;
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

   --  Cached device info
   Device_Model  : String (1 .. 32) := (others => ' ');
   Device_Serial : String (1 .. 32) := (others => ' ');
   Model_Len     : Natural := 0;
   Serial_Len    : Natural := 0;

   --  Model addresses (discovered during init)
   Inverter_Addr : Register_Address := 0;
   Storage_Addr  : Register_Address := 0;
   MPPT_Addr     : Register_Address := 0;
   MPPT_Len      : Natural := 0;
   Has_Storage   : Boolean := False;
   Has_MPPT      : Boolean := False;

   --  Current values
   AC_Power      : Float := 0.0;
   AC_Voltage    : Float := 0.0;
   AC_Current    : Float := 0.0;
   AC_Frequency  : Float := 0.0;
   DC_Power      : Float := 0.0;
   DC_Voltage    : Float := 0.0;
   Total_Energy  : Float := 0.0;
   Cabinet_Temp  : Float := 0.0;
   Op_State      : Natural := 0;

   --  Battery values
   Bat_SOC       : Float := 0.0;
   Bat_Power     : Float := 0.0;
   Bat_Status    : Storage.Storage_Status := Storage.Off;

   --  MPPT values
   type MPPT_String_Data is record
      Power   : Float := 0.0;
      Voltage : Float := 0.0;
      Current : Float := 0.0;
   end record;
   MPPT_Strings : array (1 .. 4) of MPPT_String_Data;
   MPPT_Count   : Natural := 0;

   --  Helper: Read registers
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

   --  State name lookup
   function State_Name (Code : Natural) return String is
   begin
      return (case Code is
                 when 1 => "Off         ",
                 when 2 => "Sleeping    ",
                 when 3 => "Starting    ",
                 when 4 => "Running     ",
                 when 5 => "Throttled   ",
                 when 6 => "Shutting Dwn",
                 when 7 => "FAULT       ",
                 when 8 => "Standby     ",
                 when others => "Unknown     ");
   end State_Name;

   function State_Color (Code : Natural) return Color is
   begin
      return (case Code is
                 when 4 => Bright_Green,
                 when 7 => Red,
                 when 5 => Yellow,
                 when 1 | 2 | 8 => White,
                 when others => Cyan);
   end State_Color;

   --  Draw static dashboard layout
   procedure Draw_Layout (Host : String; Port : Natural; Unit : Unit_Id) is
   begin
      Clear_Screen;
      Hide_Cursor;

      --  Main box
      Draw_Box (1, 1, Width, Height, " KOSTAL INVERTER DASHBOARD ");

      --  Header
      Print (2, Left_Col, Host & ":" & Port'Image & " (Unit" & Unit'Image & ")",
             FG => Cyan);

      if Model_Len > 0 then
         Print (3, Left_Col, Device_Model (1 .. Model_Len), FG => White, S => Bold);
      end if;
      if Serial_Len > 0 then
         Print (3, Right_Col, "S/N: " & Device_Serial (1 .. Serial_Len), FG => Bright_Black);
      end if;

      Draw_HLine (4, 2, Width - 2);

      --  Section headers
      Print (5, Left_Col, "AC OUTPUT", FG => Bright_Cyan, S => Bold);
      Print (5, Right_Col, "DC INPUT", FG => Bright_Cyan, S => Bold);

      Print (6, Left_Col, "Power:");
      Print (7, Left_Col, "Voltage:");
      Print (8, Left_Col, "Current:");
      Print (9, Left_Col, "Frequency:");

      Print (6, Right_Col, "DC Power:");
      Print (7, Right_Col, "DC Voltage:");

      Draw_HLine (10, 2, Width - 2);

      if Has_MPPT then
         Print (11, Left_Col, "MPPT STRINGS", FG => Bright_Cyan, S => Bold);
         for I in 1 .. Natural'Min (MPPT_Count, 4) loop
            Print (11 + I, Left_Col, "String" & I'Image & ":");
         end loop;
      end if;

      if Has_Storage then
         Print (11, Right_Col, "BATTERY", FG => Bright_Cyan, S => Bold);
         Print (12, Right_Col, "SOC:");
         Print (13, Right_Col, "Power:");
         Print (14, Right_Col, "Status:");
      end if;

      Draw_HLine (16, 2, Width - 2);

      Print (17, Left_Col, "ENERGY", FG => Bright_Cyan, S => Bold);
      Print (18, Left_Col, "Total:");

      Print (17, Right_Col, "STATUS", FG => Bright_Cyan, S => Bold);
      Print (18, Right_Col, "State:");
      Print (19, Right_Col, "Temp:");

      Draw_HLine (20, 2, Width - 2);

      Print (21, Left_Col, "Press Ctrl+C to exit", FG => Bright_Black);
   end Draw_Layout;

   --  Update dynamic values
   procedure Update_Values (Unit : Unit_Id) is
      AC_Regs     : Register_Array (0 .. 15);
      DC_Regs     : Register_Array (0 .. 5);
      Energy_Regs : Register_Array (0 .. 2);
      State_Regs  : Register_Array (0 .. 1);
      Temp_Regs   : Register_Array (0 .. 4);
   begin
      --  AC measurements
      if Inverter_Addr > 0 and then
         Read_Regs (Unit, Inverter_Addr + Reg_AC_Current, 16, AC_Regs)
      then
         declare
            I_SF : constant Scale_Factor := To_Scale_Factor (AC_Regs (4));
            V_SF : constant Scale_Factor := To_Scale_Factor (AC_Regs (11));
            P_SF : constant Scale_Factor := To_Scale_Factor (AC_Regs (13));
            F_SF : constant Scale_Factor := To_Scale_Factor (AC_Regs (15));
         begin
            AC_Current   := Apply_Scale (AC_Regs (0), I_SF);
            AC_Voltage   := Apply_Scale (AC_Regs (8), V_SF);
            AC_Power     := Apply_Scale (AC_Regs (12), P_SF);
            AC_Frequency := Apply_Scale (AC_Regs (14), F_SF);
         end;
      end if;

      --  DC measurements
      if Inverter_Addr > 0 and then
         Read_Regs (Unit, Inverter_Addr + Reg_DC_Current, 6, DC_Regs)
      then
         declare
            V_SF : constant Scale_Factor := To_Scale_Factor (DC_Regs (3));
            W_SF : constant Scale_Factor := To_Scale_Factor (DC_Regs (5));
         begin
            if Is_Implemented (DC_Regs (2)) then
               DC_Voltage := Apply_Scale (DC_Regs (2), V_SF);
            end if;
            if Is_Implemented (DC_Regs (4)) then
               DC_Power := Apply_Scale (DC_Regs (4), W_SF);
            end if;
         end;
      end if;

      --  Energy
      if Inverter_Addr > 0 and then
         Read_Regs (Unit, Inverter_Addr + Reg_AC_Energy, 3, Energy_Regs)
      then
         declare
            Energy_Raw : constant Unsigned_32 :=
              Unsigned_32 (Energy_Regs (0)) * 65536 + Unsigned_32 (Energy_Regs (1));
            Energy_SF  : constant Scale_Factor := To_Scale_Factor (Energy_Regs (2));
         begin
            Total_Energy := Float (Energy_Raw) * Scale_Multipliers (Energy_SF);
         end;
      end if;

      --  Temperature
      if Inverter_Addr > 0 and then
         Read_Regs (Unit, Inverter_Addr + Reg_Cabinet_Temp, 5, Temp_Regs)
      then
         declare
            Temp_SF : constant Scale_Factor := To_Scale_Factor (Temp_Regs (4));
         begin
            Cabinet_Temp := Apply_Scale_Signed (Temp_Regs (0), Temp_SF);
         end;
      end if;

      --  State
      if Inverter_Addr > 0 and then
         Read_Regs (Unit, Inverter_Addr + Reg_Operating_State, 2, State_Regs)
      then
         Op_State := Natural (State_Regs (0));
      end if;

      --  Battery
      if Has_Storage then
         declare
            Status_Regs : Register_Array (0 .. 5);
         begin
            if Read_Regs (Unit, Storage_Addr + Storage.Reg_SOC, 6, Status_Regs) then
               declare
                  SOC_SF : constant Scale_Factor := To_Scale_Factor (Status_Regs (1));
                  St_Val : constant Natural := Natural (Status_Regs (5));
               begin
                  Bat_SOC := Apply_Scale (Status_Regs (0), SOC_SF);
                  Bat_Status := (case St_Val is
                                    when 1 => Storage.Off,
                                    when 2 => Storage.Empty,
                                    when 3 => Storage.Discharging,
                                    when 4 => Storage.Charging,
                                    when 5 => Storage.Full,
                                    when 6 => Storage.Holding,
                                    when 7 => Storage.Testing,
                                    when others => Storage.Off);
               end;
            end if;
         end;
         --  Battery power from InOut register if available
         Bat_Power := AC_Power - DC_Power;  --  Approximate
      end if;

      --  MPPT strings
      if Has_MPPT and MPPT_Len > 0 then
         declare
            Header_Regs : Register_Array (0 .. 7);
            Module_Regs : Register_Array (0 .. MPPT_Module_Size - 1);
            Header      : MPPT_Header;
            Module      : MPPT_Module_Data;
            Module_Base : Register_Address := MPPT_Addr + 10;
         begin
            if Read_Regs (Unit, MPPT_Addr + MPPT_Reg_DCA_SF, 8, Header_Regs) then
               Decode_MPPT_Header (Header_Regs, Header);
               MPPT_Count := Header.Num_Modules;

               for M in 1 .. Natural'Min (Header.Num_Modules, 4) loop
                  exit when Natural (Module_Base - MPPT_Addr) + MPPT_Module_Size > MPPT_Len + 2;

                  if Read_Regs (Unit, Module_Base, MPPT_Module_Size, Module_Regs) then
                     Decode_MPPT_Module (Module_Regs, Header, Module);
                     if Module.Is_Valid then
                        MPPT_Strings (M).Power   := Module.Power_W;
                        MPPT_Strings (M).Voltage := Module.Voltage_V;
                        MPPT_Strings (M).Current := Module.Current_A;
                     end if;
                  end if;

                  Module_Base := Module_Base + MPPT_Module_Size;
               end loop;
            end if;
         end;
      end if;
   end Update_Values;

   --  Display current values
   procedure Display_Values is
      use Ada.Calendar.Formatting;
      Now : constant String := Image (Ada.Calendar.Clock);
   begin
      --  AC values
      Print (6, Left_Col + Val_Offset, Fmt_Power (AC_Power) & "      ",
             FG => Power_Color (AC_Power), S => Bold);
      Print (7, Left_Col + Val_Offset, Fmt (AC_Voltage, 1) & " V    ", FG => White);
      Print (8, Left_Col + Val_Offset, Fmt (AC_Current, 2) & " A    ", FG => White);
      Print (9, Left_Col + Val_Offset, Fmt (AC_Frequency, 2) & " Hz  ", FG => White);

      --  DC values
      Print (6, Right_Col + Val_Offset - 2, Fmt_Power (DC_Power) & "      ",
             FG => Bright_Yellow, S => Bold);
      Print (7, Right_Col + Val_Offset - 2, Fmt (DC_Voltage, 1) & " V    ", FG => White);

      --  MPPT strings
      if Has_MPPT then
         for I in 1 .. Natural'Min (MPPT_Count, 4) loop
            Print (11 + I, Left_Col + Val_Offset,
                   Fmt (MPPT_Strings (I).Power, 0) & " W  " &
                   Fmt (MPPT_Strings (I).Voltage, 0) & " V    ",
                   FG => Bright_Yellow);
         end loop;
      end if;

      --  Battery
      if Has_Storage then
         Print (12, Right_Col + Val_Offset - 4, Fmt (Bat_SOC, 1) & " %    ",
                FG => SOC_Color (Bat_SOC), S => Bold);
         Draw_Progress_Bar (12, Right_Col + Val_Offset + 6, 10, Bat_SOC,
                            FG => SOC_Color (Bat_SOC));
         Print (13, Right_Col + Val_Offset - 4, Fmt_Power (Bat_Power) & "      ",
                FG => Power_Color (-Bat_Power));
         Print (14, Right_Col + Val_Offset - 4, Bat_Status'Image & "      ", FG => Cyan);
      end if;

      --  Energy
      Print (18, Left_Col + Val_Offset - 4, Fmt_Energy (Total_Energy) & "      ",
             FG => Bright_Green, S => Bold);

      --  Status
      Print (18, Right_Col + Val_Offset - 4, State_Name (Op_State),
             FG => State_Color (Op_State), S => Bold);
      Print (19, Right_Col + Val_Offset - 4, Fmt (Cabinet_Temp, 1) & " C    ", FG => White);

      --  Timestamp
      Print (21, Right_Col, "Updated: " & Now (12 .. 19), FG => Bright_Black);
   end Display_Values;

   --  Discover models
   procedure Discover_Models (Unit : Unit_Id) is
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
                  if Read_Regs (Unit, M_Start + Reg_Serial, 16, Str_Regs) then
                     Decode_String (Str_Regs, Str_Val, Serial_Len);
                     Serial_Len := Natural'Min (Serial_Len, 32);
                     Device_Serial (1 .. Serial_Len) := Str_Val (1 .. Serial_Len);
                  end if;

               when Model_Inverter_1P | Model_Inverter_SP | Model_Inverter_3P =>
                  Inverter_Addr := M_Start;

               when Model_Storage =>
                  Storage_Addr := M_Start;
                  Has_Storage := True;

               when Model_MPPT =>
                  MPPT_Addr := M_Start;
                  MPPT_Len := M_Len;
                  Has_MPPT := True;

               when others =>
                  null;
            end case;

            Advance_Model_Iterator (Iterator, Model_Length (M_Len));
         end;
      end loop;
   end Discover_Models;

   --  Main variables
   Host     : String (1 .. 64) := (others => ' ');
   Host_Len : Natural := 0;
   Port     : Natural := Default_Port;
   Unit     : Unit_Id := Default_Unit_Id;
   Result   : Status;

begin
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("Kostal Dashboard - Live inverter monitoring");
      Put_Line ("Usage: kostal_dashboard <ip-address> [port] [unit-id]");
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
         Put_Line ("SunSpec not found at address 40000");
         Disconnect (Connection);
         return;
      end if;
   end;

   --  Discover available models
   Discover_Models (Unit);

   if Inverter_Addr = 0 then
      Put_Line ("No inverter model found");
      Disconnect (Connection);
      return;
   end if;

   --  Draw static layout
   Draw_Layout (Host (1 .. Host_Len), Port, Unit);

   --  Main loop
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
end Kostal_Dashboard;
