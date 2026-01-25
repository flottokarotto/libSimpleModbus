--  Go_E_Dashboard - Live Dashboard for go-e Charger Wallbox
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Terminal-based live dashboard for go-e Charger wallboxes.
--  Shows real-time charging status, power consumption, and controls.
--
--  Usage: go_e_dashboard <ip-address> [port]
--
--  Controls (while running):
--    +/- : Increase/decrease charging current (6-32A)
--    s   : Start charging (Force On)
--    p   : Pause charging (Force Off)
--    n   : Neutral (charger logic)
--    q   : Quit

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Exceptions;
with Interfaces; use Interfaces;

with Ada_Modbus;
with Ada_Modbus.Master;
with Ada_Modbus.Transport.TCP;
with Ada_Modbus.Energy.Go_E;

with Terminal_Dashboard; use Terminal_Dashboard;

procedure Go_E_Dashboard is

   use Ada_Modbus;
   use Ada_Modbus.Transport.TCP;
   use Ada_Modbus.Energy.Go_E;

   --  Dashboard layout
   Width      : constant := 72;
   Height     : constant := 24;
   Left_Col   : constant := 3;
   Right_Col  : constant := 40;
   Val_Offset : constant := 16;

   --  go-e defaults
   Default_Port : constant := 502;

   --  Connection
   type Connection_Access is access all TCP_Connection;
   Connection : aliased TCP_Connection;
   Conn_Ptr   : constant Connection_Access := Connection'Access;

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

   --  Current state
   Charger_Car        : Car_State := Unknown;
   Charger_Error      : Natural := 0;
   Cable_Amps         : Natural := 0;
   Current_Amps       : Natural := 16;  --  Measured value (can be outside 6-32)
   Force_Mode         : Force_State := Neutral;

   --  Measurements
   Voltage_L1, Voltage_L2, Voltage_L3 : Natural := 0;
   Current_L1, Current_L2, Current_L3 : Natural := 0;  --  x0.1A
   Power_Total   : Natural := 0;  --  x0.01W
   Power_L1, Power_L2, Power_L3 : Natural := 0;  --  x0.1kW
   Energy_Total  : Natural := 0;  --  x0.1kWh
   Energy_Session : Natural := 0;  --  Deka-Watt-Seconds

   --  Read input registers (FC 04)
   function Read_Input_Regs
     (Slave    : Unit_Id;
      Address  : Register_Address;
      Quantity : Natural;
      Values   : out Register_Array) return Boolean
   is
      Result : Status;
   begin
      Result := Modbus_Master.Read_Input_Registers
        (Ctx, Slave, Address, Register_Count (Quantity), Values);
      return Result = Success;
   end Read_Input_Regs;

   --  Read holding registers (FC 03)
   function Read_Holding_Regs
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
   end Read_Holding_Regs;

   --  Write single register (FC 06)
   function Write_Reg
     (Slave   : Unit_Id;
      Address : Register_Address;
      Value   : Register_Value) return Boolean
   is
      Result : Status;
   begin
      Result := Modbus_Master.Write_Single_Register
        (Ctx, Slave, Address, Value);
      return Result = Success;
   end Write_Reg;

   --  Get car state description
   function Car_State_Text (State : Car_State) return String is
   begin
      case State is
         when Unknown         => return "Unknown        ";
         when Idle            => return "Idle (no car)  ";
         when Charging        => return "CHARGING       ";
         when Waiting_For_Car => return "Ready to charge";
         when Charge_Complete => return "Complete       ";
      end case;
   end Car_State_Text;

   --  Get car state color
   function Car_State_Color (State : Car_State) return Color is
   begin
      case State is
         when Unknown         => return Bright_Black;
         when Idle            => return White;
         when Charging        => return Bright_Green;
         when Waiting_For_Car => return Yellow;
         when Charge_Complete => return Cyan;
      end case;
   end Car_State_Color;

   --  Get force state description
   function Force_State_Text (State : Force_State) return String is
   begin
      case State is
         when Neutral   => return "Neutral (auto)";
         when Force_Off => return "PAUSED        ";
         when Force_On  => return "FORCED ON     ";
      end case;
   end Force_State_Text;

   --  Get force state color
   function Force_State_Color (State : Force_State) return Color is
   begin
      case State is
         when Neutral   => return White;
         when Force_Off => return Yellow;
         when Force_On  => return Bright_Green;
      end case;
   end Force_State_Color;

   --  Get error description
   function Error_Text (Code : Natural) return String is
   begin
      case Code is
         when 0 => return "None           ";
         when 1 => return "RCCB/FI Error! ";
         when 3 => return "Phase Error!   ";
         when 8 => return "No Ground!     ";
         when others => return "Error " & Code'Image & "       ";
      end case;
   end Error_Text;

   --  Decode uint32 from two registers (Big Endian / Word Swap)
   function Get_Uint32 (Regs : Register_Array; Offset : Natural) return Natural is
   begin
      return Natural (Regs (Offset)) * 65536 + Natural (Regs (Offset + 1));
   end Get_Uint32;

   --  Draw static layout
   procedure Draw_Layout (Host : String; Port : Natural) is
   begin
      Clear_Screen;
      Hide_Cursor;

      --  Main box
      Draw_Box (1, 1, Width, Height, " go-e CHARGER DASHBOARD ");

      --  Connection info
      Print (2, Left_Col, "go-e Charger @ " & Host & ":" & Port'Image,
             FG => Cyan);

      Draw_HLine (3, 2, Width - 2);

      --  Status section
      Print (4, Left_Col, "STATUS", FG => Bright_Cyan, S => Bold);
      Print (5, Left_Col, "Car State:");
      Print (6, Left_Col, "Force Mode:");
      Print (7, Left_Col, "Cable:");
      Print (8, Left_Col, "Error:");

      Print (4, Right_Col, "CONTROLS", FG => Bright_Cyan, S => Bold);
      Print (5, Right_Col, "[+/-] Current:");
      Print (6, Right_Col, "[s] Start, [p] Pause");
      Print (7, Right_Col, "[n] Neutral mode");
      Print (8, Right_Col, "[q] Quit");

      Draw_HLine (9, 2, Width - 2);

      --  Power section
      Print (10, Left_Col, "POWER", FG => Bright_Cyan, S => Bold);
      Print (11, Left_Col, "Total:");
      Print (12, Left_Col, "L1:");
      Print (13, Left_Col, "L2:");
      Print (14, Left_Col, "L3:");

      Print (10, Right_Col, "VOLTAGE (L-N)", FG => Bright_Cyan, S => Bold);
      --  Labels printed with values in Display_Values to avoid overwrite

      Draw_HLine (15, 2, Width - 2);

      --  Current section
      Print (16, Left_Col, "CURRENT (A)", FG => Bright_Cyan, S => Bold);
      Print (17, Left_Col, "L1:");
      Print (18, Left_Col, "L2:");
      Print (19, Left_Col, "L3:");

      Print (16, Right_Col, "ENERGY", FG => Bright_Cyan, S => Bold);
      Print (17, Right_Col, "This Session:");
      Print (18, Right_Col, "Total:");

      Draw_HLine (20, 2, Width - 2);

      --  Power bar
      Print (21, Left_Col, "Power:", FG => White);

      Draw_HLine (22, 2, Width - 2);
      Print (23, Left_Col, "Press [q] to quit", FG => Bright_Black);
   end Draw_Layout;

   --  Update values from charger
   procedure Update_Values is
      Input_Regs   : Register_Array (0 .. 59);
      Holding_Regs : Register_Array (0 .. 124);
   begin
      --  Read input registers (30101-30160)
      if Read_Input_Regs (Default_Device_Id, Reg_Car_State, 60, Input_Regs) then
         --  Car State (offset 0 = reg 30101)
         case Input_Regs (0) is
            when 1 => Charger_Car := Idle;
            when 2 => Charger_Car := Charging;
            when 3 => Charger_Car := Waiting_For_Car;
            when 4 => Charger_Car := Charge_Complete;
            when others => Charger_Car := Unknown;
         end case;

         --  Cable amps (offset 1 = reg 30102)
         Cable_Amps := Natural (Input_Regs (1));

         --  Error (offset 7 = reg 30108)
         Charger_Error := Natural (Input_Regs (7));

         --  Voltages (offset 8-13 = regs 30109-30114, each 2 regs)
         Voltage_L1 := Get_Uint32 (Input_Regs, 8);
         Voltage_L2 := Get_Uint32 (Input_Regs, 10);
         Voltage_L3 := Get_Uint32 (Input_Regs, 12);

         --  Currents (offset 14-19 = regs 30115-30120, each 2 regs, x0.1A)
         Current_L1 := Get_Uint32 (Input_Regs, 14);
         Current_L2 := Get_Uint32 (Input_Regs, 16);
         Current_L3 := Get_Uint32 (Input_Regs, 18);

         --  Power total (offset 20-21 = regs 30121-30122, x0.01W)
         Power_Total := Get_Uint32 (Input_Regs, 20);

         --  Energy total (offset 28-29 = regs 30129-30130, x0.1kWh)
         Energy_Total := Get_Uint32 (Input_Regs, 28);

         --  Session energy (offset 32-33 = regs 30133-30134, Deka-Watt-Seconds)
         Energy_Session := Get_Uint32 (Input_Regs, 32);

         --  Power per phase (offset 46-51 = regs 30147-30152, x0.1kW)
         Power_L1 := Get_Uint32 (Input_Regs, 46);
         Power_L2 := Get_Uint32 (Input_Regs, 48);
         Power_L3 := Get_Uint32 (Input_Regs, 50);
      end if;

      --  Read holding registers for settings
      if Read_Holding_Regs (Default_Device_Id, Reg_Allow, 125, Holding_Regs) then
         --  Ampere volatile (offset 99 = reg 40300)
         Current_Amps := Natural (Holding_Regs (99));
         if Current_Amps < 6 then
            Current_Amps := 6;
         elsif Current_Amps > 32 then
            Current_Amps := 32;
         end if;

         --  Force state (offset 137 = reg 40338)
         case Holding_Regs (124) is  --  Approximate offset
            when 0 => Force_Mode := Neutral;
            when 1 => Force_Mode := Force_Off;
            when 2 => Force_Mode := Force_On;
            when others => Force_Mode := Neutral;
         end case;
      end if;
   end Update_Values;

   --  Display current values
   procedure Display_Values is
      use Ada.Calendar.Formatting;
      Now : constant String := Image (Ada.Calendar.Clock);

      Power_Watts : constant Float := Float (Power_Total) / 100.0;
      Power_kW    : constant Float := Power_Watts / 1000.0;
      --  Max power based on cable rating (not current limit) for useful bar display
      Max_Power   : constant Float := Float (Natural'Max (Cable_Amps, 16)) * 230.0 * 3.0 / 1000.0;
      Power_Pct   : Float := 0.0;

      Session_Wh : constant Float := Float (Energy_Session) * 10.0 / 3600.0;
      Total_kWh  : constant Float := Float (Energy_Total) / 10.0;
   begin
      --  Status
      Print (5, Left_Col + Val_Offset, Car_State_Text (Charger_Car),
             FG => Car_State_Color (Charger_Car), S => Bold);
      Print (6, Left_Col + Val_Offset, Force_State_Text (Force_Mode),
             FG => Force_State_Color (Force_Mode));

      if Cable_Amps > 0 then
         Print (7, Left_Col + Val_Offset, Cable_Amps'Image & " A cable     ",
                FG => Green);
      else
         Print (7, Left_Col + Val_Offset, "No cable       ", FG => Bright_Black);
      end if;

      Print (8, Left_Col + Val_Offset, Error_Text (Charger_Error),
             FG => (if Charger_Error = 0 then Green else Bright_Red));

      --  Current setting
      Print (5, Right_Col + 14, Current_Amps'Image & " A  ",
             FG => Cyan, S => Bold);

      --  Power (values limited to not overwrite voltage column)
      Print (11, Left_Col + Val_Offset,
             Fmt (Power_kW, 2) & " kW ",
             FG => Power_Color (Power_Watts), S => Bold);
      Print (12, Left_Col + Val_Offset,
             Fmt (Float (Power_L1) * 100.0, 0) & " W ",
             FG => Power_Color (Float (Power_L1) * 100.0));
      Print (13, Left_Col + Val_Offset,
             Fmt (Float (Power_L2) * 100.0, 0) & " W ",
             FG => Power_Color (Float (Power_L2) * 100.0));
      Print (14, Left_Col + Val_Offset,
             Fmt (Float (Power_L3) * 100.0, 0) & " W ",
             FG => Power_Color (Float (Power_L3) * 100.0));

      --  Voltage (L-N) - direct from registers
      Print (11, Right_Col, "L1:" & Voltage_L1'Image & " V  ", FG => White);
      Print (12, Right_Col, "L2:" & Voltage_L2'Image & " V  ", FG => White);
      Print (13, Right_Col, "L3:" & Voltage_L3'Image & " V  ", FG => White);

      --  Current
      Print (17, Left_Col + Val_Offset - 10,
             Fmt (Float (Current_L1) / 10.0, 1) & "    ", FG => Cyan);
      Print (18, Left_Col + Val_Offset - 10,
             Fmt (Float (Current_L2) / 10.0, 1) & "    ", FG => Cyan);
      Print (19, Left_Col + Val_Offset - 10,
             Fmt (Float (Current_L3) / 10.0, 1) & "    ", FG => Cyan);

      --  Energy
      if Session_Wh >= 1000.0 then
         Print (17, Right_Col + 14,
                Fmt (Session_Wh / 1000.0, 2) & " kWh    ", FG => Yellow);
      else
         Print (17, Right_Col + 14,
                Fmt (Session_Wh, 0) & " Wh    ", FG => Yellow);
      end if;
      Print (18, Right_Col + 14, Fmt (Total_kWh, 1) & " kWh    ", FG => Yellow);

      --  Power bar
      if Max_Power > 0.0 then
         Power_Pct := Power_kW / Max_Power * 100.0;
         if Power_Pct > 100.0 then
            Power_Pct := 100.0;
         end if;
      end if;
      Draw_Progress_Bar (21, Left_Col + 8, Width - 12, Power_Pct,
                         FG => (if Charger_Car = Charging then Green else Bright_Black));
      Print (21, Width - 8, Fmt (Power_Pct, 0) & "%  ", FG => White);

      --  Timestamp
      Print (23, Right_Col, "Updated: " & Now (12 .. 19), FG => Bright_Black);
   end Display_Values;

   --  Process keyboard input
   procedure Handle_Input is
      Available : Boolean;
      Key       : Character;
      Dummy     : Boolean;
   begin
      Ada.Text_IO.Get_Immediate (Key, Available);
      if Available then
         case Key is
            when '+' | '=' =>
               if Current_Amps < 32 then
                  Dummy := Write_Reg (Default_Device_Id, Reg_Ampere_Volatile,
                                      Register_Value (Current_Amps + 1));
               end if;

            when '-' | '_' =>
               if Current_Amps > 6 then
                  Dummy := Write_Reg (Default_Device_Id, Reg_Ampere_Volatile,
                                      Register_Value (Current_Amps - 1));
               end if;

            when 's' | 'S' =>
               --  Start charging (Force On)
               Dummy := Write_Reg (Default_Device_Id, Reg_Force_State, 2);

            when 'p' | 'P' =>
               --  Pause charging (Force Off)
               Dummy := Write_Reg (Default_Device_Id, Reg_Force_State, 1);

            when 'n' | 'N' =>
               --  Neutral mode
               Dummy := Write_Reg (Default_Device_Id, Reg_Force_State, 0);

            when 'q' | 'Q' | ASCII.ESC =>
               raise Program_Error with "User quit";

            when others =>
               null;
         end case;
      end if;
   exception
      when Ada.Text_IO.End_Error =>
         null;  --  No input available
   end Handle_Input;

   --  Main variables
   Host     : String (1 .. 64) := (others => ' ');
   Host_Len : Natural := 0;
   Port     : Natural := Default_Port;
   Result   : Status;

begin
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("go-e Charger Dashboard - Live wallbox monitoring");
      New_Line;
      Put_Line ("Usage: go_e_dashboard <ip-address> [port]");
      Put_Line ("  Default port: 502");
      New_Line;
      Put_Line ("Controls:");
      Put_Line ("  +/-  Increase/decrease current (6-32A)");
      Put_Line ("  s    Start charging (Force On)");
      Put_Line ("  p    Pause charging (Force Off)");
      Put_Line ("  n    Neutral mode (charger logic)");
      Put_Line ("  q    Quit");
      New_Line;
      Put_Line ("Note: Modbus TCP must be enabled in go-e app!");
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

   Put_Line ("Connecting to go-e Charger at " & Host (1 .. Host_Len) & ":" & Port'Image & "...");
   Put_Line ("(Make sure Modbus TCP is enabled in go-e app)");

   Connect (Connection, Host (1 .. Host_Len), Port, 5.0, Result);
   if Result /= Success then
      Put_Line ("Connection failed: " & Result'Image);
      Put_Line ("Check: 1) IP address  2) Modbus enabled in app  3) Port 502");
      return;
   end if;

   Modbus_Master.Initialize
     (Ctx,
      (Mode            => Modbus_Master.TCP,
       Default_Slave   => Default_Device_Id,
       Default_Timeout => 2000),
      Conn_Ptr);

   --  Test connection by reading car state
   declare
      Test_Regs : Register_Array (0 .. 0);
   begin
      if not Read_Input_Regs (Default_Device_Id, Reg_Car_State, 1, Test_Regs) then
         Put_Line ("Failed to read from go-e Charger");
         Put_Line ("Device might not support Modbus or wrong address");
         Disconnect (Connection);
         return;
      end if;
   end;

   Draw_Layout (Host (1 .. Host_Len), Port);

   loop
      Update_Values;
      Display_Values;
      Handle_Input;
      delay 0.5;  --  Update every 500ms
   end loop;

exception
   when E : others =>
      Show_Cursor;
      Reset_Style;
      Move_To (Height + 1, 1);
      declare
         Msg : constant String := Ada.Exceptions.Exception_Message (E);
      begin
         if Msg /= "User quit" then
            Put_Line ("Exit: " & Msg);
         else
            Put_Line ("Dashboard closed.");
         end if;
      end;
      Disconnect (Connection);
end Go_E_Dashboard;
