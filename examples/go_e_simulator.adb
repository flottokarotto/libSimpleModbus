--  Go_E_Simulator - Simulated go-e Charger Wallbox
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Simulates a go-e Charger wallbox via Modbus TCP:
--  - Realistic charging simulation (voltage, current, power)
--  - Responds to control commands (force state, ampere limit)
--  - Simulates car connect/disconnect cycles
--
--  Usage: go_e_simulator [port]
--  Default port: 502
--
--  Test with: go_e_dashboard 127.0.0.1 [port]

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Calendar; use Ada.Calendar;
with Ada.Numerics.Float_Random;
with Interfaces; use Interfaces;

with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Slave; use Ada_Modbus.Slave;
with Ada_Modbus.Transport.TCP; use Ada_Modbus.Transport.TCP;
with Ada_Modbus.Protocol.TCP;
with Ada_Modbus.Energy.Go_E; use Ada_Modbus.Energy.Go_E;

procedure Go_E_Simulator is

   -----------------------
   --  Simulation State --
   -----------------------

   --  Charger state
   type Sim_Car_State is (No_Car, Car_Connected, Charging, Charge_Complete);

   Sim_State        : Sim_Car_State := No_Car;
   Sim_Force        : Force_State := Neutral;
   Sim_Allow        : Boolean := True;
   Sim_Ampere_Limit : Natural := 16;
   Sim_Cable_Amps   : constant Natural := 32;  --  Simulated 32A cable
   Sim_Error        : constant Natural := 0;

   --  Charging simulation
   Sim_SOC          : Float := 20.0;   --  Simulated battery SOC %
   Sim_Target_SOC   : Float := 80.0;   --  Target SOC
   Sim_Energy_Total : Unsigned_32 := 12345;  --  x0.1 kWh (1234.5 kWh)
   Sim_Energy_Sess  : Unsigned_32 := 0;      --  Deka-Watt-Seconds
   Sim_Phase_Count  : constant Natural := 3;  --  3-phase charging

   --  Timing
   Last_Update      : Time;
   Request_Count    : Natural := 0;

   --  Random generator for realistic values
   Gen : Ada.Numerics.Float_Random.Generator;

   --  Simulated register values (calculated on read)
   Voltage_L1, Voltage_L2, Voltage_L3 : Natural := 230;
   Current_L1, Current_L2, Current_L3 : Natural := 0;  --  x0.1A
   Power_Total : Unsigned_32 := 0;   --  x0.01W
   Power_L1, Power_L2, Power_L3 : Unsigned_32 := 0;  --  x0.1kW

   -----------------------
   --  Register Arrays  --
   -----------------------

   --  Input Registers (FC 04) - 30xxx series, offset from 100
   --  We map addresses relative to 100 (so 30101 = index 0)
   Input_Registers : Register_Array (0 .. 199) := [others => 0];

   --  Holding Registers (FC 03/06) - 40xxx series, offset from 200
   --  We map addresses relative to 200 (so 40201 = index 0)
   Holding_Registers : Register_Array (0 .. 199) := [others => 0];

   --------------------------
   --  Simulation Update   --
   --------------------------

   procedure Update_Simulation is
      use Ada.Numerics.Float_Random;
      Now     : constant Time := Clock;
      Elapsed : Duration;
      Dt      : Float;
      Noise   : Float;
      Charging_Current : Float;
      Power_Per_Phase  : Float;
   begin
      Elapsed := Now - Last_Update;
      Dt := Float (Elapsed);
      Last_Update := Now;

      --  Add some noise to voltage (228-232V typical)
      Noise := Random (Gen) * 4.0 - 2.0;
      Voltage_L1 := Natural (230.0 + Noise);
      Noise := Random (Gen) * 4.0 - 2.0;
      Voltage_L2 := Natural (230.0 + Noise);
      Noise := Random (Gen) * 4.0 - 2.0;
      Voltage_L3 := Natural (230.0 + Noise);

      --  State machine for car simulation
      case Sim_State is
         when No_Car =>
            --  Randomly connect a car (every ~30 seconds on average)
            if Random (Gen) < 0.03 * Dt then
               Sim_State := Car_Connected;
               Sim_SOC := 20.0 + Random (Gen) * 30.0;  --  20-50% SOC
               Sim_Target_SOC := 80.0;
               Sim_Energy_Sess := 0;
               Put_Line ("[SIM] Car connected (SOC:" & Integer (Sim_SOC)'Image & "%)");
            end if;
            Current_L1 := 0;
            Current_L2 := 0;
            Current_L3 := 0;

         when Car_Connected =>
            --  Start charging if allowed
            if Sim_Allow and Sim_Force /= Force_Off then
               if Sim_Force = Force_On or Random (Gen) < 0.5 * Dt then
                  Sim_State := Charging;
                  Put_Line ("[SIM] Charging started");
               end if;
            end if;
            --  Car might disconnect
            if Random (Gen) < 0.01 * Dt then
               Sim_State := No_Car;
               Put_Line ("[SIM] Car disconnected");
            end if;
            Current_L1 := 0;
            Current_L2 := 0;
            Current_L3 := 0;

         when Charging =>
            --  Check if should stop
            if not Sim_Allow or Sim_Force = Force_Off then
               Sim_State := Car_Connected;
               Put_Line ("[SIM] Charging paused");
            elsif Sim_SOC >= Sim_Target_SOC then
               Sim_State := Charge_Complete;
               Put_Line ("[SIM] Charging complete (SOC:" & Integer (Sim_SOC)'Image & "%)");
            else
               --  Simulate charging
               --  Current ramps up/down with some variation
               Charging_Current := Float (Sim_Ampere_Limit);
               --  Reduce current as SOC increases (simulate battery)
               if Sim_SOC > 70.0 then
                  Charging_Current := Charging_Current * (1.0 - (Sim_SOC - 70.0) / 30.0 * 0.5);
               end if;
               --  Add small noise
               Charging_Current := Charging_Current * (0.98 + Random (Gen) * 0.04);

               --  Set currents (x0.1A) - 3-phase charging
               Current_L1 := Natural (Charging_Current * 10.0);
               Current_L2 := Natural (Charging_Current * 10.0 * (0.98 + Random (Gen) * 0.04));
               Current_L3 := Natural (Charging_Current * 10.0 * (0.98 + Random (Gen) * 0.04));

               --  Update SOC (assume ~50kWh battery)
               Power_Per_Phase := Charging_Current * 230.0;
               Sim_SOC := Sim_SOC + (Power_Per_Phase * Float (Sim_Phase_Count) / 50000.0) * Dt / 3600.0 * 100.0;

               --  Update session energy (Deka-Watt-Seconds)
               Sim_Energy_Sess := Sim_Energy_Sess +
                 Unsigned_32 (Power_Per_Phase * Float (Sim_Phase_Count) * Dt / 10.0);

               --  Update total energy (x0.1 kWh)
               Sim_Energy_Total := Sim_Energy_Total +
                 Unsigned_32 (Power_Per_Phase * Float (Sim_Phase_Count) * Dt / 3600.0 / 100.0);
            end if;

         when Charge_Complete =>
            --  Car might disconnect
            if Random (Gen) < 0.02 * Dt then
               Sim_State := No_Car;
               Put_Line ("[SIM] Car disconnected after charge");
            end if;
            Current_L1 := 0;
            Current_L2 := 0;
            Current_L3 := 0;
      end case;

      --  Calculate power values
      --  Power per phase (x0.1 kW = 100W units)
      Power_L1 := Unsigned_32 (Voltage_L1 * Current_L1 / 1000);
      Power_L2 := Unsigned_32 (Voltage_L2 * Current_L2 / 1000);
      Power_L3 := Unsigned_32 (Voltage_L3 * Current_L3 / 1000);

      --  Total power (x0.01W)
      Power_Total := Unsigned_32 (
        Voltage_L1 * Current_L1 +
        Voltage_L2 * Current_L2 +
        Voltage_L3 * Current_L3);

      --  Update Input Registers
      --  Car State (30101 = index 0)
      Input_Registers (0) := (case Sim_State is
                                 when No_Car => 1,
                                 when Car_Connected => 3,
                                 when Charging => 2,
                                 when Charge_Complete => 4);

      --  Cable amps (30102 = index 1)
      Input_Registers (1) := Register_Value (Sim_Cable_Amps);

      --  Error (30108 = index 7)
      Input_Registers (7) := Register_Value (Sim_Error);

      --  Voltages (30109-30114 = indices 8-13, 2 regs each)
      Input_Registers (8) := 0;
      Input_Registers (9) := Register_Value (Voltage_L1);
      Input_Registers (10) := 0;
      Input_Registers (11) := Register_Value (Voltage_L2);
      Input_Registers (12) := 0;
      Input_Registers (13) := Register_Value (Voltage_L3);

      --  Currents (30115-30120 = indices 14-19, 2 regs each, x0.1A)
      Input_Registers (14) := 0;
      Input_Registers (15) := Register_Value (Current_L1);
      Input_Registers (16) := 0;
      Input_Registers (17) := Register_Value (Current_L2);
      Input_Registers (18) := 0;
      Input_Registers (19) := Register_Value (Current_L3);

      --  Power Total (30121-30122 = indices 20-21, x0.01W)
      Input_Registers (20) := Register_Value (Shift_Right (Power_Total, 16) and 16#FFFF#);
      Input_Registers (21) := Register_Value (Power_Total and 16#FFFF#);

      --  Energy Total (30129-30130 = indices 28-29, x0.1kWh)
      Input_Registers (28) := Register_Value (Shift_Right (Sim_Energy_Total, 16) and 16#FFFF#);
      Input_Registers (29) := Register_Value (Sim_Energy_Total and 16#FFFF#);

      --  Energy Session (30133-30134 = indices 32-33, Deka-Watt-Seconds)
      Input_Registers (32) := Register_Value (Shift_Right (Sim_Energy_Sess, 16) and 16#FFFF#);
      Input_Registers (33) := Register_Value (Sim_Energy_Sess and 16#FFFF#);

      --  Power per phase (30147-30152 = indices 46-51, x0.1kW)
      Input_Registers (46) := Register_Value (Shift_Right (Power_L1, 16) and 16#FFFF#);
      Input_Registers (47) := Register_Value (Power_L1 and 16#FFFF#);
      Input_Registers (48) := Register_Value (Shift_Right (Power_L2, 16) and 16#FFFF#);
      Input_Registers (49) := Register_Value (Power_L2 and 16#FFFF#);
      Input_Registers (50) := Register_Value (Shift_Right (Power_L3, 16) and 16#FFFF#);
      Input_Registers (51) := Register_Value (Power_L3 and 16#FFFF#);

      --  Update Holding Registers
      --  Allow (40201 = index 0)
      Holding_Registers (0) := (if Sim_Allow then 1 else 0);

      --  Ampere Volatile (40300 = index 99)
      Holding_Registers (99) := Register_Value (Sim_Ampere_Limit);

      --  Force State (40338 = index 137)
      Holding_Registers (137) := (case Sim_Force is
                                     when Neutral => 0,
                                     when Force_Off => 1,
                                     when Force_On => 2);
   end Update_Simulation;

   ----------------------
   --  Slave Callbacks --
   ----------------------

   function Read_Input_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      --  go-e uses 30xxx addresses, we offset by 100
      Start : constant Integer := Integer (Start_Address) - 100;
   begin
      Update_Simulation;

      if Start < 0 or Start + Natural (Quantity) > Input_Registers'Length then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Input_Registers (Start + I);
      end loop;
      return Success;
   end Read_Input_Registers_CB;

   function Read_Holding_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      --  go-e uses 40xxx addresses, we offset by 200
      Start : constant Integer := Integer (Start_Address) - 200;
   begin
      Update_Simulation;

      if Start < 0 or Start + Natural (Quantity) > Holding_Registers'Length then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Holding_Registers (Start + I);
      end loop;
      return Success;
   end Read_Holding_Registers_CB;

   function Write_Single_Register_CB
     (Address : Register_Address;
      Value   : Register_Value) return Status
   is
      --  go-e uses 40xxx addresses, we offset by 200
      Addr : constant Integer := Integer (Address) - 200;
   begin
      if Addr < 0 or Addr > Holding_Registers'Last then
         return Exception_Illegal_Address;
      end if;

      Holding_Registers (Addr) := Value;

      --  Handle specific registers
      case Addr is
         when 0 =>  --  Allow (40201)
            Sim_Allow := Value /= 0;
            Put_Line ("[SIM] Allow set to " & Sim_Allow'Image);

         when 99 =>  --  Ampere Volatile (40300)
            if Natural (Value) >= 6 and Natural (Value) <= 32 then
               Sim_Ampere_Limit := Natural (Value);
               Put_Line ("[SIM] Ampere limit set to" & Sim_Ampere_Limit'Image & " A");
            end if;

         when 137 =>  --  Force State (40338)
            case Value is
               when 0 => Sim_Force := Neutral;
               when 1 => Sim_Force := Force_Off;
               when 2 => Sim_Force := Force_On;
               when others => null;
            end case;
            Put_Line ("[SIM] Force state set to " & Sim_Force'Image);

         when others =>
            null;
      end case;

      return Success;
   end Write_Single_Register_CB;

   function Write_Multiple_Registers_CB
     (Start_Address : Register_Address;
      Values        : Register_Array) return Status
   is
      Start : constant Integer := Integer (Start_Address) - 200;
      Res   : Status;
   begin
      if Start < 0 or Start + Values'Length > Holding_Registers'Length then
         return Exception_Illegal_Address;
      end if;

      --  Write each register individually to trigger handlers
      for I in Values'Range loop
         Res := Write_Single_Register_CB
           (Register_Address (200 + Start + I - Values'First), Values (I));
         if Res /= Success then
            return Res;
         end if;
      end loop;

      return Success;
   end Write_Multiple_Registers_CB;

   --  Null handlers for unsupported functions
   function Null_Read_Coils
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
      pragma Unreferenced (Start_Address, Quantity);
   begin
      Values := [others => False];
      return Exception_Illegal_Function;
   end Null_Read_Coils;

   function Null_Write_Coil
     (Address : Coil_Address;
      Value   : Coil_Value) return Status
   is
      pragma Unreferenced (Address, Value);
   begin
      return Exception_Illegal_Function;
   end Null_Write_Coil;

   ----------------------
   --  Main Program    --
   ----------------------

   Config : constant Slave_Config :=
     (Mode      => TCP,
      Unit_Id   => Default_Device_Id,
      Callbacks =>
        (Read_Holding_Registers   => Read_Holding_Registers_CB'Unrestricted_Access,
         Read_Input_Registers     => Read_Input_Registers_CB'Unrestricted_Access,
         Write_Single_Register    => Write_Single_Register_CB'Unrestricted_Access,
         Write_Multiple_Registers => Write_Multiple_Registers_CB'Unrestricted_Access,
         Read_Coils               => Null_Read_Coils'Unrestricted_Access,
         Read_Discrete_Inputs     => Null_Read_Coils'Unrestricted_Access,
         Write_Single_Coil        => Null_Write_Coil'Unrestricted_Access,
         Write_Multiple_Coils     => null,
         Read_Exception_Status    => null,
         Diagnostics              => null,
         Report_Server_Id         => null,
         Mask_Write_Register      => null,
         Read_Write_Registers     => null));

   Server    : aliased TCP_Connection;
   Client    : aliased TCP_Connection;
   Port      : Natural := 502;
   Result    : Status;
   Has_Client : Boolean := False;

   Request_Buffer  : Byte_Array (0 .. Protocol.TCP.Max_ADU_Size - 1) := [others => 0];
   Response_Buffer : Byte_Array (0 .. Protocol.TCP.Max_ADU_Size - 1) := [others => 0];
   Request_Length  : Natural;
   Response_Length : Natural;
   Send_Response   : Boolean;

begin
   Put_Line ("+--------------------------------------------------+");
   Put_Line ("|           go-e Charger Simulator                 |");
   Put_Line ("+--------------------------------------------------+");
   New_Line;

   --  Initialize random generator
   Ada.Numerics.Float_Random.Reset (Gen);

   --  Parse arguments
   if Ada.Command_Line.Argument_Count >= 1 then
      Port := Natural'Value (Ada.Command_Line.Argument (1));
   end if;

   Put_Line ("Port:        " & Port'Image);
   Put_Line ("Unit ID:     " & Default_Device_Id'Image);
   Put_Line ("Cable:       32A (simulated)");
   Put_Line ("Phases:      3");
   New_Line;
   Put_Line ("Simulation:");
   Put_Line ("  - Car connects/disconnects randomly");
   Put_Line ("  - Charging simulates 50kWh battery");
   Put_Line ("  - SOC increases during charging");
   Put_Line ("  - Responds to force state commands");
   New_Line;

   Last_Update := Clock;

   --  Start server
   Listen (Server, Port, Result);
   if Result /= Success then
      Put_Line ("ERROR: Failed to start server: " & Result'Image);
      Put_Line ("(Port 502 may require admin rights, try 1502)");
      return;
   end if;

   Put_Line ("Server listening on port" & Port'Image);
   Put_Line ("Test with: go_e_dashboard 127.0.0.1" & Port'Image);
   Put_Line ("Press Ctrl+C to stop.");
   New_Line;

   --  Main loop
   loop
      Update_Simulation;

      if not Has_Client then
         Accept_Connection (Server, Client, Result);
         if Result = Success then
            Has_Client := True;
            Put_Line ("[NET] Client connected");
         end if;
      end if;

      if Has_Client then
         Request_Length := Receive (Client, Request_Buffer,
                                    Protocol.TCP.Max_ADU_Size, 100);

         if Request_Length > 0 then
            Request_Count := Request_Count + 1;

            Process_Request (Config,
                             Request_Buffer, Request_Length,
                             Response_Buffer, Response_Length,
                             Send_Response);

            if Send_Response and Response_Length > 0 then
               declare
                  Dummy : constant Natural :=
                    Send (Client, Response_Buffer (0 .. Response_Length - 1));
                  pragma Unreferenced (Dummy);
               begin
                  null;  --  Ignore send errors
               end;
            end if;
         end if;
      end if;
   end loop;

exception
   when E : others =>
      Put_Line ("Exception: " & Ada.Exceptions.Exception_Message (E));
      Put_Line ("Requests processed:" & Request_Count'Image);
      Disconnect (Client);
      Disconnect (Server);
end Go_E_Simulator;
