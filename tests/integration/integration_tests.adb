--  Integration_Tests - Modbus TCP Integration Test Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  This test suite connects to a real Modbus TCP server (pymodbus simulator)
--  and tests all function codes supported by the AdaModbus library.
--
--  Test data expected from simulator (modbus_simulator.py):
--    - Coils: Alternating ON/OFF (even addresses = ON)
--    - Discrete Inputs: Every 3rd address is ON
--    - Holding Registers: addr 0-9 = addr*100, addr 10 = 0x1234, etc.
--    - Input Registers: value = address + 1000

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Calendar;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Master;
with Ada_Modbus.Transport.TCP;

package body Integration_Tests is

   --  Configuration - adjust these if needed
   Simulator_Host : constant String := "localhost";
   Simulator_Port : constant Natural := 5020;
   Test_Slave_Id  : constant Unit_Id := 1;
   Test_Timeout   : constant Natural := 3000;  -- 3 seconds

   use Ada_Modbus.Transport.TCP;

   --  Connection and access type for transport context
   type Connection_Access is access all TCP_Connection;
   Connection : aliased TCP_Connection;
   Conn_Ptr   : constant Connection_Access := Connection'Access;

   --  Track connection state
   Is_Connected : Boolean := False;

   --  Transport callbacks for Master generic
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
      Now     : constant Time := Clock;
      Seconds : constant Duration := Now - Time_Of (1970, 1, 1, 0.0);
   begin
      return Unsigned_32 (Seconds * 1000.0) mod Unsigned_32'Last;
   end Get_Tick;

   --  Instantiate Master with TCP transport
   package TCP_Master is new Ada_Modbus.Master
     (Transport_Context => Connection_Access,
      Send              => Send_Data,
      Receive           => Receive_Data,
      Get_Tick_Ms       => Get_Tick);

   Ctx : TCP_Master.Master_Context;

   --  Helper: Ensure connection to simulator
   procedure Ensure_Connected is
      Result : Status;
   begin
      if Is_Connected then
         return;
      end if;

      Connect (Connection, Simulator_Host, Simulator_Port, 5.0, Result);

      if Result /= Success then
         Ada.Text_IO.Put_Line ("WARNING: Could not connect to simulator at " &
                               Simulator_Host & ":" & Simulator_Port'Image);
         Ada.Text_IO.Put_Line ("Make sure modbus_simulator.py is running!");
         Ada.Text_IO.Put_Line ("Error: " & Last_Error (Connection));
      else
         TCP_Master.Initialize
           (Ctx,
            (Mode => TCP_Master.TCP,
             Default_Slave => Test_Slave_Id,
             Default_Timeout => Test_Timeout),
            Conn_Ptr);
         Is_Connected := True;
      end if;
   end Ensure_Connected;

   --  Helper: Skip test if not connected
   procedure Skip_If_Not_Connected (T : Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Ensure_Connected;
      if not Is_Connected then
         --  AUnit doesn't have skip, so we just note it
         Assert (True, "SKIPPED: Simulator not available");
      end if;
   end Skip_If_Not_Connected;

   type Integration_Test_Case is new Test_Case with null record;

   overriding function Name (T : Integration_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("Modbus TCP Integration Tests"));

   overriding procedure Register_Tests (T : in Out Integration_Test_Case);
   overriding procedure Set_Up (T : in Out Integration_Test_Case);

   overriding procedure Set_Up (T : in Out Integration_Test_Case) is
      pragma Unreferenced (T);
   begin
      Ensure_Connected;
   end Set_Up;

   --=========================================================================
   --  FC 01: Read Coils
   --=========================================================================

   procedure Test_FC01_Read_Coils (T : in Out Test_Case'Class);
   procedure Test_FC01_Read_Coils (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Values : Coil_Array (0 .. 7);
      Result : Status;
   begin
      if not Is_Connected then
         return;
      end if;

      Result := TCP_Master.Read_Coils
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 0, Quantity => 8, Values => Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC01 Read Coils failed: " & Result'Image);

      --  Verify pattern: even addresses are ON
      Assert (Values (0) = True, "Coil 0 should be ON");
      Assert (Values (1) = False, "Coil 1 should be OFF");
      Assert (Values (2) = True, "Coil 2 should be ON");
      Assert (Values (3) = False, "Coil 3 should be OFF");
      Assert (Values (4) = True, "Coil 4 should be ON");
      Assert (Values (5) = False, "Coil 5 should be OFF");
      Assert (Values (6) = True, "Coil 6 should be ON");
      Assert (Values (7) = False, "Coil 7 should be OFF");
   end Test_FC01_Read_Coils;

   --=========================================================================
   --  FC 02: Read Discrete Inputs
   --=========================================================================

   procedure Test_FC02_Read_Discrete_Inputs (T : in Out Test_Case'Class);
   procedure Test_FC02_Read_Discrete_Inputs (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Values : Coil_Array (0 .. 8);
      Result : Status;
   begin
      if not Is_Connected then
         return;
      end if;

      Result := TCP_Master.Read_Discrete_Inputs
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 0, Quantity => 9, Values => Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC02 Read Discrete Inputs failed: " & Result'Image);

      --  Verify pattern: addresses divisible by 3 are ON
      Assert (Values (0) = True, "Input 0 should be ON (0 mod 3 = 0)");
      Assert (Values (1) = False, "Input 1 should be OFF");
      Assert (Values (2) = False, "Input 2 should be OFF");
      Assert (Values (3) = True, "Input 3 should be ON (3 mod 3 = 0)");
      Assert (Values (4) = False, "Input 4 should be OFF");
      Assert (Values (5) = False, "Input 5 should be OFF");
      Assert (Values (6) = True, "Input 6 should be ON (6 mod 3 = 0)");
   end Test_FC02_Read_Discrete_Inputs;

   --=========================================================================
   --  FC 03: Read Holding Registers
   --=========================================================================

   procedure Test_FC03_Read_Holding_Registers (T : in Out Test_Case'Class);
   procedure Test_FC03_Read_Holding_Registers (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Values : Register_Array (0 .. 4);
      Result : Status;
   begin
      if not Is_Connected then
         return;
      end if;

      --  Read addresses 0-4 (sequential pattern: 0, 100, 200, 300, 400)
      Result := TCP_Master.Read_Holding_Registers
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 0, Quantity => 5, Values => Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC03 Read Holding Registers failed: " & Result'Image);

      Assert (Values (0) = 0, "Register 0 should be 0");
      Assert (Values (1) = 100, "Register 1 should be 100");
      Assert (Values (2) = 200, "Register 2 should be 200");
      Assert (Values (3) = 300, "Register 3 should be 300");
      Assert (Values (4) = 400, "Register 4 should be 400");
   end Test_FC03_Read_Holding_Registers;

   procedure Test_FC03_Read_Test_Values (T : in Out Test_Case'Class);
   procedure Test_FC03_Read_Test_Values (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Values : Register_Array (0 .. 5);
      Result : Status;
   begin
      if not Is_Connected then
         return;
      end if;

      --  Read addresses 10-15 (test values)
      Result := TCP_Master.Read_Holding_Registers
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 10, Quantity => 6, Values => Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC03 Read Test Values failed: " & Result'Image);

      Assert (Values (0) = 16#1234#, "Register 10 should be 0x1234");
      Assert (Values (1) = 16#5678#, "Register 11 should be 0x5678");
      Assert (Values (2) = 16#9ABC#, "Register 12 should be 0x9ABC");
      Assert (Values (3) = 16#DEF0#, "Register 13 should be 0xDEF0");
      Assert (Values (4) = 16#AAAA#, "Register 14 should be 0xAAAA");
      Assert (Values (5) = 16#5555#, "Register 15 should be 0x5555");
   end Test_FC03_Read_Test_Values;

   --=========================================================================
   --  FC 04: Read Input Registers
   --=========================================================================

   procedure Test_FC04_Read_Input_Registers (T : in Out Test_Case'Class);
   procedure Test_FC04_Read_Input_Registers (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Values : Register_Array (0 .. 4);
      Result : Status;
   begin
      if not Is_Connected then
         return;
      end if;

      --  Read addresses 0-4 (pattern: value = address + 1000)
      Result := TCP_Master.Read_Input_Registers
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 0, Quantity => 5, Values => Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC04 Read Input Registers failed: " & Result'Image);

      Assert (Values (0) = 1000, "Input Register 0 should be 1000");
      Assert (Values (1) = 1001, "Input Register 1 should be 1001");
      Assert (Values (2) = 1002, "Input Register 2 should be 1002");
      Assert (Values (3) = 1003, "Input Register 3 should be 1003");
      Assert (Values (4) = 1004, "Input Register 4 should be 1004");
   end Test_FC04_Read_Input_Registers;

   --=========================================================================
   --  FC 05: Write Single Coil
   --=========================================================================

   procedure Test_FC05_Write_Single_Coil (T : in Out Test_Case'Class);
   procedure Test_FC05_Write_Single_Coil (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Read_Values : Coil_Array (0 .. 0);
      Result : Status;
   begin
      if not Is_Connected then
         return;
      end if;

      --  Write ON to coil 50 (initially OFF since 50 is even)
      Result := TCP_Master.Write_Single_Coil
        (Ctx, Slave => Test_Slave_Id,
         Address => 50, Value => True,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC05 Write Single Coil failed: " & Result'Image);

      --  Verify the write by reading back
      Result := TCP_Master.Read_Coils
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 50, Quantity => 1, Values => Read_Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC01 Read after FC05 failed: " & Result'Image);
      Assert (Read_Values (0) = True, "Coil 50 should now be ON after write");

      --  Restore original value
      Result := TCP_Master.Write_Single_Coil
        (Ctx, Slave => Test_Slave_Id,
         Address => 50, Value => False,
         Timeout_Ms => Test_Timeout);
   end Test_FC05_Write_Single_Coil;

   --=========================================================================
   --  FC 06: Write Single Register
   --=========================================================================

   procedure Test_FC06_Write_Single_Register (T : in Out Test_Case'Class);
   procedure Test_FC06_Write_Single_Register (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Read_Values : Register_Array (0 .. 0);
      Result : Status;
      Original_Value : Register_Value;
   begin
      if not Is_Connected then
         return;
      end if;

      --  Read original value at address 100
      Result := TCP_Master.Read_Holding_Registers
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 100, Quantity => 1, Values => Read_Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC03 Read before FC06 failed: " & Result'Image);
      Original_Value := Read_Values (0);

      --  Write a test value
      Result := TCP_Master.Write_Single_Register
        (Ctx, Slave => Test_Slave_Id,
         Address => 100, Value => 16#CAFE#,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC06 Write Single Register failed: " & Result'Image);

      --  Verify the write by reading back
      Result := TCP_Master.Read_Holding_Registers
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 100, Quantity => 1, Values => Read_Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC03 Read after FC06 failed: " & Result'Image);
      Assert (Read_Values (0) = 16#CAFE#, "Register 100 should be 0xCAFE after write");

      --  Restore original value
      Result := TCP_Master.Write_Single_Register
        (Ctx, Slave => Test_Slave_Id,
         Address => 100, Value => Original_Value,
         Timeout_Ms => Test_Timeout);
   end Test_FC06_Write_Single_Register;

   --=========================================================================
   --  FC 15: Write Multiple Coils
   --=========================================================================

   procedure Test_FC15_Write_Multiple_Coils (T : in Out Test_Case'Class);
   procedure Test_FC15_Write_Multiple_Coils (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Write_Values : Coil_Array (0 .. 7) := [True, True, True, True,
                                              False, False, False, False];
      Read_Values : Coil_Array (0 .. 7);
      Result : Status;
   begin
      if not Is_Connected then
         return;
      end if;

      --  Write pattern to coils 60-67
      Result := TCP_Master.Write_Multiple_Coils
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 60, Values => Write_Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC15 Write Multiple Coils failed: " & Result'Image);

      --  Verify the write by reading back
      Result := TCP_Master.Read_Coils
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 60, Quantity => 8, Values => Read_Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC01 Read after FC15 failed: " & Result'Image);

      for I in 0 .. 3 loop
         Assert (Read_Values (I) = True,
                 "Coil" & Natural'Image (60 + I) & " should be ON");
      end loop;
      for I in 4 .. 7 loop
         Assert (Read_Values (I) = False,
                 "Coil" & Natural'Image (60 + I) & " should be OFF");
      end loop;
   end Test_FC15_Write_Multiple_Coils;

   --=========================================================================
   --  FC 16: Write Multiple Registers
   --=========================================================================

   procedure Test_FC16_Write_Multiple_Registers (T : in Out Test_Case'Class);
   procedure Test_FC16_Write_Multiple_Registers (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Write_Values : Register_Array (0 .. 4) := [16#1111#, 16#2222#, 16#3333#,
                                                  16#4444#, 16#5555#];
      Read_Values : Register_Array (0 .. 4);
      Result : Status;
   begin
      if not Is_Connected then
         return;
      end if;

      --  Write to registers 120-124
      Result := TCP_Master.Write_Multiple_Registers
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 120, Values => Write_Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC16 Write Multiple Registers failed: " & Result'Image);

      --  Verify the write by reading back
      Result := TCP_Master.Read_Holding_Registers
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 120, Quantity => 5, Values => Read_Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC03 Read after FC16 failed: " & Result'Image);

      Assert (Read_Values (0) = 16#1111#, "Register 120 should be 0x1111");
      Assert (Read_Values (1) = 16#2222#, "Register 121 should be 0x2222");
      Assert (Read_Values (2) = 16#3333#, "Register 122 should be 0x3333");
      Assert (Read_Values (3) = 16#4444#, "Register 123 should be 0x4444");
      Assert (Read_Values (4) = 16#5555#, "Register 124 should be 0x5555");
   end Test_FC16_Write_Multiple_Registers;

   --=========================================================================
   --  FC 22: Mask Write Register
   --=========================================================================

   procedure Test_FC22_Mask_Write_Register (T : in Out Test_Case'Class);
   procedure Test_FC22_Mask_Write_Register (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Read_Values : Register_Array (0 .. 0);
      Result : Status;
   begin
      if not Is_Connected then
         return;
      end if;

      --  First, set register 130 to a known value
      Result := TCP_Master.Write_Single_Register
        (Ctx, Slave => Test_Slave_Id,
         Address => 130, Value => 16#FF00#,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC06 setup for FC22 failed: " & Result'Image);

      --  Apply mask: Result = (Current AND And_Mask) OR (Or_Mask AND (NOT And_Mask))
      --  Current = 0xFF00
      --  And_Mask = 0xF0F0 (keep these bits)
      --  Or_Mask = 0x0F0F (set these bits where And_Mask is 0)
      --  Expected = (0xFF00 AND 0xF0F0) OR (0x0F0F AND NOT 0xF0F0)
      --           = 0xF000 OR (0x0F0F AND 0x0F0F)
      --           = 0xF000 OR 0x0F0F
      --           = 0xFF0F
      Result := TCP_Master.Mask_Write_Register
        (Ctx, Slave => Test_Slave_Id,
         Address => 130,
         And_Mask => 16#F0F0#,
         Or_Mask => 16#0F0F#,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC22 Mask Write Register failed: " & Result'Image);

      --  Verify the result
      Result := TCP_Master.Read_Holding_Registers
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 130, Quantity => 1, Values => Read_Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC03 Read after FC22 failed: " & Result'Image);
      Assert (Read_Values (0) = 16#FF0F#,
              "Register 130 should be 0xFF0F after mask write, got " &
              Register_Value'Image (Read_Values (0)));
   end Test_FC22_Mask_Write_Register;

   --=========================================================================
   --  FC 23: Read/Write Multiple Registers
   --=========================================================================

   procedure Test_FC23_Read_Write_Multiple_Registers (T : in Out Test_Case'Class);
   procedure Test_FC23_Read_Write_Multiple_Registers (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Write_Values : Register_Array (0 .. 2) := [16#AAAA#, 16#BBBB#, 16#CCCC#];
      Read_Values : Register_Array (0 .. 2);
      Result : Status;
   begin
      if not Is_Connected then
         return;
      end if;

      --  Write to 140-142 and read from 10-12 in a single transaction
      Result := TCP_Master.Read_Write_Multiple_Registers
        (Ctx, Slave => Test_Slave_Id,
         Read_Start => 10,
         Read_Quantity => 3,
         Read_Values => Read_Values,
         Write_Start => 140,
         Write_Values => Write_Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success,
              "FC23 Read/Write Multiple Registers failed: " & Result'Image);

      --  Verify read values (from address 10: 0x1234, 0x5678, 0x9ABC)
      Assert (Read_Values (0) = 16#1234#, "Read value 0 should be 0x1234");
      Assert (Read_Values (1) = 16#5678#, "Read value 1 should be 0x5678");
      Assert (Read_Values (2) = 16#9ABC#, "Read value 2 should be 0x9ABC");

      --  Verify write was successful by reading back
      Result := TCP_Master.Read_Holding_Registers
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 140, Quantity => 3, Values => Read_Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success, "FC03 Read after FC23 failed: " & Result'Image);
      Assert (Read_Values (0) = 16#AAAA#, "Register 140 should be 0xAAAA");
      Assert (Read_Values (1) = 16#BBBB#, "Register 141 should be 0xBBBB");
      Assert (Read_Values (2) = 16#CCCC#, "Register 142 should be 0xCCCC");
   end Test_FC23_Read_Write_Multiple_Registers;

   --=========================================================================
   --  Test: Maximum Register Count
   --=========================================================================

   procedure Test_Max_Register_Read (T : in Out Test_Case'Class);
   procedure Test_Max_Register_Read (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Values : Register_Array (0 .. 124);  --  125 registers (max per Modbus spec)
      Result : Status;
   begin
      if not Is_Connected then
         return;
      end if;

      --  Read maximum number of registers
      Result := TCP_Master.Read_Holding_Registers
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 0, Quantity => 125, Values => Values,
         Timeout_Ms => Test_Timeout);

      Assert (Result = Success,
              "Reading max 125 registers failed: " & Result'Image);

      --  Verify some values
      Assert (Values (0) = 0, "Register 0 should be 0");
      Assert (Values (10) = 16#1234#, "Register 10 should be 0x1234");
   end Test_Max_Register_Read;

   --=========================================================================
   --  Test: Invalid Address (Exception Response)
   --=========================================================================

   procedure Test_Invalid_Address (T : in Out Test_Case'Class);
   procedure Test_Invalid_Address (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Values : Register_Array (0 .. 9);
      Result : Status;
   begin
      if not Is_Connected then
         return;
      end if;

      --  Try to read from a very high address (likely out of range)
      Result := TCP_Master.Read_Holding_Registers
        (Ctx, Slave => Test_Slave_Id,
         Start_Address => 50000, Quantity => 10, Values => Values,
         Timeout_Ms => Test_Timeout);

      --  Should get an exception response (Illegal Address)
      Assert (Result = Exception_Illegal_Address or else
              Result = Exception_Illegal_Value,
              "Expected exception for invalid address, got: " & Result'Image);
   end Test_Invalid_Address;

   --=========================================================================
   --  Test: Connection to Non-Existent Slave
   --=========================================================================

   procedure Test_Wrong_Slave_Id (T : in Out Test_Case'Class);
   procedure Test_Wrong_Slave_Id (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Values : Register_Array (0 .. 0);
      Result : Status;
   begin
      if not Is_Connected then
         return;
      end if;

      --  Try to communicate with a different slave ID
      --  Note: pymodbus might respond anyway, or timeout
      Result := TCP_Master.Read_Holding_Registers
        (Ctx, Slave => 99,  --  Non-existent slave
         Start_Address => 0, Quantity => 1, Values => Values,
         Timeout_Ms => 1000);  --  Short timeout

      --  The result depends on simulator behavior:
      --  - Timeout if slave doesn't respond
      --  - Success if simulator responds to all slave IDs
      --  Either is acceptable for this test
      Assert (Result = Success or else
              Result = Timeout or else
              Result = Exception_Gateway_Target,
              "Unexpected result for wrong slave ID: " & Result'Image);
   end Test_Wrong_Slave_Id;

   --=========================================================================
   --  Registration
   --=========================================================================

   overriding procedure Register_Tests (T : in Out Integration_Test_Case) is
   begin
      --  Core function codes
      Registration.Register_Routine
        (T, Test_FC01_Read_Coils'Access, "FC01 Read Coils");
      Registration.Register_Routine
        (T, Test_FC02_Read_Discrete_Inputs'Access, "FC02 Read Discrete Inputs");
      Registration.Register_Routine
        (T, Test_FC03_Read_Holding_Registers'Access, "FC03 Read Holding Registers");
      Registration.Register_Routine
        (T, Test_FC03_Read_Test_Values'Access, "FC03 Read Test Values");
      Registration.Register_Routine
        (T, Test_FC04_Read_Input_Registers'Access, "FC04 Read Input Registers");
      Registration.Register_Routine
        (T, Test_FC05_Write_Single_Coil'Access, "FC05 Write Single Coil");
      Registration.Register_Routine
        (T, Test_FC06_Write_Single_Register'Access, "FC06 Write Single Register");
      Registration.Register_Routine
        (T, Test_FC15_Write_Multiple_Coils'Access, "FC15 Write Multiple Coils");
      Registration.Register_Routine
        (T, Test_FC16_Write_Multiple_Registers'Access, "FC16 Write Multiple Registers");

      --  Extended function codes
      Registration.Register_Routine
        (T, Test_FC22_Mask_Write_Register'Access, "FC22 Mask Write Register");
      Registration.Register_Routine
        (T, Test_FC23_Read_Write_Multiple_Registers'Access, "FC23 Read/Write Multiple Registers");

      --  Edge cases
      Registration.Register_Routine
        (T, Test_Max_Register_Read'Access, "Max Register Read (125)");
      Registration.Register_Routine
        (T, Test_Invalid_Address'Access, "Invalid Address (Exception)");
      Registration.Register_Routine
        (T, Test_Wrong_Slave_Id'Access, "Wrong Slave ID");
   end Register_Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new Integration_Test_Case);
      return S;
   end Suite;

end Integration_Tests;
