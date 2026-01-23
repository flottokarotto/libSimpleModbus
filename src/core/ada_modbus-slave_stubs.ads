--  Ada_Modbus.Slave_Stubs - Default stub implementations
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides default implementations for all Modbus function handlers.
--  Each stub returns Exception_Illegal_Function to indicate the function
--  is not supported. Use these when instantiating Slave_Generic for
--  functions you don't want to implement.

with Interfaces;

package Ada_Modbus.Slave_Stubs
  with SPARK_Mode => On
is

   pragma Pure;

   --  Read Coils (FC 01) - not supported
   function Null_Read_Coils
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
     with Side_Effects, Global => null;

   --  Read Discrete Inputs (FC 02) - not supported
   function Null_Read_Discrete_Inputs
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
     with Side_Effects, Global => null;

   --  Read Holding Registers (FC 03) - not supported
   function Null_Read_Holding_Registers
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
     with Side_Effects, Global => null;

   --  Read Input Registers (FC 04) - not supported
   function Null_Read_Input_Registers
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
     with Side_Effects, Global => null;

   --  Write Single Coil (FC 05) - not supported
   function Null_Write_Single_Coil
     (Address : Coil_Address;
      Value   : Coil_Value) return Status
     with Global => null;

   --  Write Single Register (FC 06) - not supported
   function Null_Write_Single_Register
     (Address : Register_Address;
      Value   : Register_Value) return Status
     with Global => null;

   --  Write Multiple Coils (FC 15) - not supported
   function Null_Write_Multiple_Coils
     (Start_Address : Coil_Address;
      Values        : Coil_Array) return Status
     with Global => null;

   --  Write Multiple Registers (FC 16) - not supported
   function Null_Write_Multiple_Registers
     (Start_Address : Register_Address;
      Values        : Register_Array) return Status
     with Global => null;

   --  Read Exception Status (FC 07) - not supported
   function Null_Read_Exception_Status
     (Exception_Status : out Byte) return Status
     with Side_Effects, Global => null;

   --  Diagnostics (FC 08) - not supported
   function Null_Diagnostics
     (Sub_Function : Interfaces.Unsigned_16;
      Data_In      : Interfaces.Unsigned_16;
      Data_Out     : out Interfaces.Unsigned_16) return Status
     with Side_Effects, Global => null;

   --  Report Server ID (FC 17) - not supported
   function Null_Report_Server_Id
     (Server_Id     : out Byte;
      Run_Indicator : out Boolean;
      Add_Data      : out Byte_Array;
      Add_Data_Len  : out Natural) return Status
     with Side_Effects, Global => null;

   --  Mask Write Register (FC 22) - not supported
   function Null_Mask_Write_Register
     (Address  : Register_Address;
      And_Mask : Register_Value;
      Or_Mask  : Register_Value) return Status
     with Global => null;

   --  Read/Write Multiple Registers (FC 23) - not supported
   function Null_Read_Write_Registers
     (Read_Start    : Register_Address;
      Read_Quantity : Register_Count;
      Read_Values   : out Register_Array;
      Write_Start   : Register_Address;
      Write_Values  : Register_Array) return Status
     with Side_Effects, Global => null;

end Ada_Modbus.Slave_Stubs;
