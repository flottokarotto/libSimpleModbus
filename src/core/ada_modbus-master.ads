--  Ada_Modbus.Master - Modbus Master (Client) implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Generic master implementation using callback-based transport abstraction.
--  Works with any transport (serial, TCP) by providing Send/Receive callbacks.

with Interfaces;
with Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.TCP;

generic
   --  Transport context type (e.g., TCP connection, serial port handle)
   type Transport_Context is private;

   --  Transport callbacks

   --  Send data over transport
   --  Returns number of bytes actually sent, 0 on error
   with function Send
     (Ctx  : in out Transport_Context;
      Data : Byte_Array) return Natural;

   --  Receive data from transport
   --  Returns number of bytes received, 0 on timeout/error
   --  Timeout_Ms: maximum time to wait for data
   with function Receive
     (Ctx        : in out Transport_Context;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural;

   --  Get current time in milliseconds (for timeout handling)
   with function Get_Tick_Ms return Interfaces.Unsigned_32;

package Ada_Modbus.Master is

   --  Protocol mode
   type Protocol_Mode is (RTU, ASCII, TCP);

   --  Master configuration
   type Master_Config is record
      Mode            : Protocol_Mode := RTU;
      Default_Slave   : Unit_Id := 1;
      Default_Timeout : Natural := 1000;  --  ms
   end record;

   --  Master context (holds state for TCP transaction IDs etc.)
   type Master_Context is record
      Config          : Master_Config;
      Transaction_Id  : Protocol.TCP.Transaction_Id := 0;
      Transport       : Transport_Context;
   end record;

   --  Initialize master context
   procedure Initialize
     (Ctx       : out Master_Context;
      Config    : Master_Config;
      Transport : Transport_Context);

   -------------------------
   --  Read Functions     --
   -------------------------

   --  Read Coils (FC 01)
   function Read_Coils
     (Ctx           : in out Master_Context;
      Slave         : Unit_Id;
      Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array;
      Timeout_Ms    : Natural := 0) return Status
     with Pre => Values'Length >= Natural (Quantity);

   --  Read Discrete Inputs (FC 02)
   function Read_Discrete_Inputs
     (Ctx           : in out Master_Context;
      Slave         : Unit_Id;
      Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array;
      Timeout_Ms    : Natural := 0) return Status
     with Pre => Values'Length >= Natural (Quantity);

   --  Read Holding Registers (FC 03)
   function Read_Holding_Registers
     (Ctx           : in out Master_Context;
      Slave         : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Timeout_Ms    : Natural := 0) return Status
     with Pre => Values'Length >= Natural (Quantity);

   --  Read Input Registers (FC 04)
   function Read_Input_Registers
     (Ctx           : in out Master_Context;
      Slave         : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Timeout_Ms    : Natural := 0) return Status
     with Pre => Values'Length >= Natural (Quantity);

   -------------------------
   --  Write Functions    --
   -------------------------

   --  Write Single Coil (FC 05)
   function Write_Single_Coil
     (Ctx        : in out Master_Context;
      Slave      : Unit_Id;
      Address    : Coil_Address;
      Value      : Coil_Value;
      Timeout_Ms : Natural := 0) return Status;

   --  Write Single Register (FC 06)
   function Write_Single_Register
     (Ctx        : in out Master_Context;
      Slave      : Unit_Id;
      Address    : Register_Address;
      Value      : Register_Value;
      Timeout_Ms : Natural := 0) return Status;

   --  Write Multiple Coils (FC 15)
   function Write_Multiple_Coils
     (Ctx           : in out Master_Context;
      Slave         : Unit_Id;
      Start_Address : Coil_Address;
      Values        : Coil_Array;
      Timeout_Ms    : Natural := 0) return Status;

   --  Write Multiple Registers (FC 16)
   function Write_Multiple_Registers
     (Ctx           : in out Master_Context;
      Slave         : Unit_Id;
      Start_Address : Register_Address;
      Values        : Register_Array;
      Timeout_Ms    : Natural := 0) return Status;

   -------------------------------
   --  Diagnostic Functions    --
   -------------------------------

   --  Read Exception Status (FC 07)
   --  Returns 8-bit exception status from slave
   function Read_Exception_Status
     (Ctx              : in out Master_Context;
      Slave            : Unit_Id;
      Exception_Status : out Byte;
      Timeout_Ms       : Natural := 0) return Status;

   --  Diagnostics (FC 08)
   --  Sub-function 0x0000 = Return Query Data (echo test)
   function Diagnostics
     (Ctx          : in out Master_Context;
      Slave        : Unit_Id;
      Sub_Function : Interfaces.Unsigned_16;
      Data_In      : Interfaces.Unsigned_16;
      Data_Out     : out Interfaces.Unsigned_16;
      Timeout_Ms   : Natural := 0) return Status;

   --  Report Server ID (FC 17)
   --  Returns slave identification and run status
   function Report_Server_Id
     (Ctx           : in out Master_Context;
      Slave         : Unit_Id;
      Server_Id     : out Byte;
      Run_Indicator : out Boolean;
      Add_Data      : out Byte_Array;
      Add_Data_Len  : out Natural;
      Timeout_Ms    : Natural := 0) return Status;

   --  Mask Write Register (FC 22)
   --  Modifies a register using AND and OR masks:
   --  Result = (Current AND And_Mask) OR (Or_Mask AND (NOT And_Mask))
   function Mask_Write_Register
     (Ctx        : in out Master_Context;
      Slave      : Unit_Id;
      Address    : Register_Address;
      And_Mask   : Register_Value;
      Or_Mask    : Register_Value;
      Timeout_Ms : Natural := 0) return Status;

   --  Read/Write Multiple Registers (FC 23)
   --  Performs a write then a read in a single transaction
   function Read_Write_Multiple_Registers
     (Ctx            : in out Master_Context;
      Slave          : Unit_Id;
      Read_Start     : Register_Address;
      Read_Quantity  : Register_Count;
      Read_Values    : out Register_Array;
      Write_Start    : Register_Address;
      Write_Values   : Register_Array;
      Timeout_Ms     : Natural := 0) return Status
     with Pre => Read_Values'Length >= Natural (Read_Quantity);

private

   --  Internal: Execute request/response transaction
   function Execute_Transaction
     (Ctx        : in out Master_Context;
      Slave      : Unit_Id;
      Request    : Protocol.PDU_Buffer;
      Req_Length : Natural;
      Response   : out Protocol.PDU_Buffer;
      Resp_Len   : out Natural;
      Timeout_Ms : Natural) return Status
     with Pre => Req_Length > 0 and then Req_Length <= Protocol.Max_PDU_Size;

end Ada_Modbus.Master;
