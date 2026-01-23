--  Ada_Modbus.Slave_Generic - SPARK-compatible generic Modbus Slave
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  This is the SPARK-compatible version of the Modbus slave implementation.
--  It uses generic formal subprogram parameters instead of callbacks,
--  making it suitable for formal verification.
--
--  Usage:
--    1. Implement functions for the Modbus operations you want to support
--    2. Use Slave_Stubs.Null_* for operations you don't support
--    3. Instantiate this package with your implementations
--
--  Example:
--    package My_Slave is new Ada_Modbus.Slave_Generic
--      (Slave_Unit_Id           => 1,
--       Read_Holding_Registers  => My_Read_Registers'Access,
--       Write_Single_Register   => My_Write_Register'Access,
--       -- ... other parameters using Slave_Stubs.Null_* ...
--      );

with Interfaces;
with Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.RTU;
with Ada_Modbus.Protocol.ASCII;
with Ada_Modbus.Protocol.TCP;
with Ada_Modbus.Slave_Stubs;

generic
   --  Slave configuration
   Slave_Unit_Id : Unit_Id;

   --  Read Coils (FC 01)
   with function Read_Coils
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
     is Slave_Stubs.Null_Read_Coils;

   --  Read Discrete Inputs (FC 02)
   with function Read_Discrete_Inputs
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
     is Slave_Stubs.Null_Read_Discrete_Inputs;

   --  Read Holding Registers (FC 03)
   with function Read_Holding_Registers
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
     is Slave_Stubs.Null_Read_Holding_Registers;

   --  Read Input Registers (FC 04)
   with function Read_Input_Registers
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
     is Slave_Stubs.Null_Read_Input_Registers;

   --  Write Single Coil (FC 05)
   with function Write_Single_Coil
     (Address : Coil_Address;
      Value   : Coil_Value) return Status
     is Slave_Stubs.Null_Write_Single_Coil;

   --  Write Single Register (FC 06)
   with function Write_Single_Register
     (Address : Register_Address;
      Value   : Register_Value) return Status
     is Slave_Stubs.Null_Write_Single_Register;

   --  Write Multiple Coils (FC 15)
   with function Write_Multiple_Coils
     (Start_Address : Coil_Address;
      Values        : Coil_Array) return Status
     is Slave_Stubs.Null_Write_Multiple_Coils;

   --  Write Multiple Registers (FC 16)
   with function Write_Multiple_Registers
     (Start_Address : Register_Address;
      Values        : Register_Array) return Status
     is Slave_Stubs.Null_Write_Multiple_Registers;

   --  Read Exception Status (FC 07)
   with function Read_Exception_Status
     (Exception_Status : out Byte) return Status
     is Slave_Stubs.Null_Read_Exception_Status;

   --  Diagnostics (FC 08)
   with function Diagnostics
     (Sub_Function : Interfaces.Unsigned_16;
      Data_In      : Interfaces.Unsigned_16;
      Data_Out     : out Interfaces.Unsigned_16) return Status
     is Slave_Stubs.Null_Diagnostics;

   --  Report Server ID (FC 17)
   with function Report_Server_Id
     (Server_Id     : out Byte;
      Run_Indicator : out Boolean;
      Add_Data      : out Byte_Array;
      Add_Data_Len  : out Natural) return Status
     is Slave_Stubs.Null_Report_Server_Id;

   --  Mask Write Register (FC 22)
   with function Mask_Write_Register
     (Address  : Register_Address;
      And_Mask : Register_Value;
      Or_Mask  : Register_Value) return Status
     is Slave_Stubs.Null_Mask_Write_Register;

   --  Read/Write Multiple Registers (FC 23)
   with function Read_Write_Registers
     (Read_Start    : Register_Address;
      Read_Quantity : Register_Count;
      Read_Values   : out Register_Array;
      Write_Start   : Register_Address;
      Write_Values  : Register_Array) return Status
     is Slave_Stubs.Null_Read_Write_Registers;

package Ada_Modbus.Slave_Generic
  with SPARK_Mode => On
is

   --  Protocol mode
   type Protocol_Mode is (RTU, ASCII_Mode, TCP);

   --  Process RTU request and generate response
   --  Returns True if response should be sent, False for broadcast
   procedure Process_RTU_Request
     (Request_Frame   : Byte_Array;
      Request_Length  : Natural;
      Response_Frame  : out Byte_Array;
      Response_Length : out Natural;
      Send_Response   : out Boolean)
     with Pre => Request_Frame'Length >= Protocol.RTU.Max_ADU_Size
                 and then Response_Frame'Length >= Protocol.RTU.Max_ADU_Size;

   --  Process ASCII request and generate response
   procedure Process_ASCII_Request
     (Request_Frame   : Byte_Array;
      Request_Length  : Natural;
      Response_Frame  : out Byte_Array;
      Response_Length : out Natural;
      Send_Response   : out Boolean)
     with Pre => Request_Frame'Length >= Protocol.ASCII.Max_Frame_Size
                 and then Response_Frame'Length >= Protocol.ASCII.Max_Frame_Size;

   --  Process TCP request and generate response
   procedure Process_TCP_Request
     (Request_Frame   : Byte_Array;
      Request_Length  : Natural;
      Response_Frame  : out Byte_Array;
      Response_Length : out Natural;
      Send_Response   : out Boolean)
     with Pre => Request_Frame'Length >= Protocol.TCP.Max_ADU_Size
                 and then Response_Frame'Length >= Protocol.TCP.Max_ADU_Size;

private

   --  Process PDU and generate response PDU
   procedure Process_PDU
     (Request_PDU  : Protocol.PDU_Buffer;
      Request_Len  : Natural;
      Response_PDU : out Protocol.PDU_Buffer;
      Response_Len : out Natural);

   --  Build exception response PDU
   procedure Build_Exception_Response
     (FC           : Function_Code;
      Exception_St : Status;
      Response     : out Protocol.PDU_Buffer;
      Response_Len : out Natural);

end Ada_Modbus.Slave_Generic;
