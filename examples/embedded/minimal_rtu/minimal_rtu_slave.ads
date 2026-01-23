--  Minimal_RTU_Slave - Minimal Modbus RTU Slave for Embedded
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  This is a minimal embedded example showing how to use AdaModbus
--  for Modbus RTU on bare-metal embedded systems without any OS or TCP/IP.
--
--  To use:
--  1. Implement UART_Send and UART_Receive for your hardware
--  2. Call Poll from your main loop
--  3. Access Holding_Registers for your application data

with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Slave; use Ada_Modbus.Slave;

package Minimal_RTU_Slave is

   --  Configuration
   Unit_Id_Value : constant Unit_Id := 1;

   --  Data storage (accessible from application)
   Holding_Registers : Register_Array (0 .. 31) := [others => 0];
   Input_Registers   : Register_Array (0 .. 15) := [others => 0];
   Coils             : Coil_Array (0 .. 15) := [others => False];

   --  Initialize the slave
   procedure Initialize;

   --  Poll for incoming requests (call from main loop)
   --  Returns True if a request was processed
   function Poll return Boolean;

   --  Statistics
   Request_Count : Natural := 0;
   Error_Count   : Natural := 0;

private

   --  Slave callbacks
   function Read_Holding_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status;

   function Read_Input_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status;

   function Write_Single_Register_CB
     (Address : Register_Address;
      Value   : Register_Value) return Status;

   function Write_Multiple_Registers_CB
     (Start_Address : Register_Address;
      Values        : Register_Array) return Status;

   function Read_Coils_CB
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status;

   function Write_Single_Coil_CB
     (Address : Coil_Address;
      Value   : Coil_Value) return Status;

end Minimal_RTU_Slave;
