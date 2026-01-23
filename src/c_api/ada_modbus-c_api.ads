--  Ada_Modbus.C_API - C language bindings for AdaModbus
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides a flat C-compatible API with:
--  - Opaque handles for connection contexts
--  - Function pointers for callbacks
--  - C-compatible basic types

with Interfaces.C;
with Interfaces.C.Strings;
with System;

package Ada_Modbus.C_API is

   use Interfaces.C;

   ---------------------
   --  C-Compatible Types
   ---------------------

   --  Status codes (same values as Ada Status enum)
   subtype C_Status is int;

   C_Success                     : constant C_Status := 0;
   C_Timeout                     : constant C_Status := 1;
   C_CRC_Error                   : constant C_Status := 2;
   C_LRC_Error                   : constant C_Status := 3;
   C_Frame_Error                 : constant C_Status := 4;
   C_Invalid_Response            : constant C_Status := 5;
   C_Invalid_Request             : constant C_Status := 6;
   C_Buffer_Too_Small            : constant C_Status := 7;
   C_Not_Implemented             : constant C_Status := 8;
   C_Exception_Illegal_Function  : constant C_Status := 9;
   C_Exception_Illegal_Address   : constant C_Status := 10;
   C_Exception_Illegal_Value     : constant C_Status := 11;
   C_Exception_Slave_Failure     : constant C_Status := 12;
   C_Exception_Acknowledge       : constant C_Status := 13;
   C_Exception_Slave_Busy        : constant C_Status := 14;
   C_Exception_Gateway_Path      : constant C_Status := 15;
   C_Exception_Gateway_Target    : constant C_Status := 16;

   --  Protocol mode
   subtype C_Protocol_Mode is int;

   C_Mode_RTU   : constant C_Protocol_Mode := 0;
   C_Mode_ASCII : constant C_Protocol_Mode := 1;
   C_Mode_TCP   : constant C_Protocol_Mode := 2;

   --  Opaque handle for TCP connection
   subtype C_TCP_Handle is System.Address;

   --  Opaque handle for master context
   subtype C_Master_Handle is System.Address;

   --  Opaque handle for slave context
   subtype C_Slave_Handle is System.Address;

   ---------------------
   --  TCP Connection API
   ---------------------

   --  Create a new TCP connection handle
   function Modbus_TCP_Create return C_TCP_Handle
     with Export, Convention => C, External_Name => "modbus_tcp_create";

   --  Destroy a TCP connection handle
   procedure Modbus_TCP_Destroy (Handle : C_TCP_Handle)
     with Export, Convention => C, External_Name => "modbus_tcp_destroy";

   --  Connect to a Modbus TCP server
   function Modbus_TCP_Connect
     (Handle     : C_TCP_Handle;
      Host       : Interfaces.C.Strings.chars_ptr;
      Port       : int;
      Timeout_Ms : int) return C_Status
     with Export, Convention => C, External_Name => "modbus_tcp_connect";

   --  Disconnect from server
   procedure Modbus_TCP_Disconnect (Handle : C_TCP_Handle)
     with Export, Convention => C, External_Name => "modbus_tcp_disconnect";

   --  Start TCP server listening
   function Modbus_TCP_Listen
     (Handle : C_TCP_Handle;
      Port   : int) return C_Status
     with Export, Convention => C, External_Name => "modbus_tcp_listen";

   --  Accept incoming connection (returns new handle for client)
   function Modbus_TCP_Accept
     (Server_Handle : C_TCP_Handle;
      Client_Handle : access C_TCP_Handle) return C_Status
     with Export, Convention => C, External_Name => "modbus_tcp_accept";

   --  Close server socket
   procedure Modbus_TCP_Close_Server (Handle : C_TCP_Handle)
     with Export, Convention => C, External_Name => "modbus_tcp_close_server";

   --  Get connection state (0=Disconnected, 1=Connected, 2=Listening, 3=Error)
   function Modbus_TCP_State (Handle : C_TCP_Handle) return int
     with Export, Convention => C, External_Name => "modbus_tcp_state";

   --  Get last error message
   function Modbus_TCP_Last_Error
     (Handle : C_TCP_Handle) return Interfaces.C.Strings.chars_ptr
     with Export, Convention => C, External_Name => "modbus_tcp_last_error";

   ---------------------
   --  Master (Client) API
   ---------------------

   --  Create a new master context
   function Modbus_Master_Create
     (TCP_Handle : C_TCP_Handle;
      Mode       : C_Protocol_Mode;
      Unit_Id    : unsigned_char;
      Timeout_Ms : int) return C_Master_Handle
     with Export, Convention => C, External_Name => "modbus_master_create";

   --  Destroy a master context
   procedure Modbus_Master_Destroy (Handle : C_Master_Handle)
     with Export, Convention => C, External_Name => "modbus_master_destroy";

   --  FC 01: Read Coils
   function Modbus_Read_Coils
     (Handle        : C_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_char;  --  Bit-packed array
      Timeout_Ms    : int) return C_Status
     with Export, Convention => C, External_Name => "modbus_read_coils";

   --  FC 02: Read Discrete Inputs
   function Modbus_Read_Discrete_Inputs
     (Handle        : C_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_char;  --  Bit-packed array
      Timeout_Ms    : int) return C_Status
     with Export, Convention => C,
          External_Name => "modbus_read_discrete_inputs";

   --  FC 03: Read Holding Registers
   function Modbus_Read_Holding_Registers
     (Handle        : C_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_short;  --  Array of 16-bit values
      Timeout_Ms    : int) return C_Status
     with Export, Convention => C,
          External_Name => "modbus_read_holding_registers";

   --  FC 04: Read Input Registers
   function Modbus_Read_Input_Registers
     (Handle        : C_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_short;  --  Array of 16-bit values
      Timeout_Ms    : int) return C_Status
     with Export, Convention => C,
          External_Name => "modbus_read_input_registers";

   --  FC 05: Write Single Coil
   function Modbus_Write_Single_Coil
     (Handle     : C_Master_Handle;
      Slave      : unsigned_char;
      Address    : unsigned_short;
      Value      : int;  --  0 = OFF, non-zero = ON
      Timeout_Ms : int) return C_Status
     with Export, Convention => C, External_Name => "modbus_write_single_coil";

   --  FC 06: Write Single Register
   function Modbus_Write_Single_Register
     (Handle     : C_Master_Handle;
      Slave      : unsigned_char;
      Address    : unsigned_short;
      Value      : unsigned_short;
      Timeout_Ms : int) return C_Status
     with Export, Convention => C,
          External_Name => "modbus_write_single_register";

   --  FC 15: Write Multiple Coils
   function Modbus_Write_Multiple_Coils
     (Handle        : C_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_char;  --  Bit-packed array
      Timeout_Ms    : int) return C_Status
     with Export, Convention => C,
          External_Name => "modbus_write_multiple_coils";

   --  FC 16: Write Multiple Registers
   function Modbus_Write_Multiple_Registers
     (Handle        : C_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_short;  --  Array of 16-bit values
      Timeout_Ms    : int) return C_Status
     with Export, Convention => C,
          External_Name => "modbus_write_multiple_registers";

   ---------------------
   --  Slave (Server) Callback Types
   ---------------------

   --  Callback for FC 01: Read Coils
   type Read_Coils_CB is access function
     (User_Data     : System.Address;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_char) return C_Status
     with Convention => C;

   --  Callback for FC 02: Read Discrete Inputs
   type Read_Discrete_Inputs_CB is access function
     (User_Data     : System.Address;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_char) return C_Status
     with Convention => C;

   --  Callback for FC 03: Read Holding Registers
   type Read_Holding_Registers_CB is access function
     (User_Data     : System.Address;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_short) return C_Status
     with Convention => C;

   --  Callback for FC 04: Read Input Registers
   type Read_Input_Registers_CB is access function
     (User_Data     : System.Address;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_short) return C_Status
     with Convention => C;

   --  Callback for FC 05: Write Single Coil
   type Write_Single_Coil_CB is access function
     (User_Data : System.Address;
      Address   : unsigned_short;
      Value     : int) return C_Status
     with Convention => C;

   --  Callback for FC 06: Write Single Register
   type Write_Single_Register_CB is access function
     (User_Data : System.Address;
      Address   : unsigned_short;
      Value     : unsigned_short) return C_Status
     with Convention => C;

   --  Callback for FC 15: Write Multiple Coils
   type Write_Multiple_Coils_CB is access function
     (User_Data     : System.Address;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_char) return C_Status
     with Convention => C;

   --  Callback for FC 16: Write Multiple Registers
   type Write_Multiple_Registers_CB is access function
     (User_Data     : System.Address;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_short) return C_Status
     with Convention => C;

   --  Callback struct for slave
   type Slave_Callbacks is record
      User_Data                : System.Address := System.Null_Address;
      Read_Coils               : Read_Coils_CB := null;
      Read_Discrete_Inputs     : Read_Discrete_Inputs_CB := null;
      Read_Holding_Registers   : Read_Holding_Registers_CB := null;
      Read_Input_Registers     : Read_Input_Registers_CB := null;
      Write_Single_Coil        : Write_Single_Coil_CB := null;
      Write_Single_Register    : Write_Single_Register_CB := null;
      Write_Multiple_Coils     : Write_Multiple_Coils_CB := null;
      Write_Multiple_Registers : Write_Multiple_Registers_CB := null;
   end record
     with Convention => C;

   ---------------------
   --  Slave (Server) API
   ---------------------

   --  Create a new slave context
   function Modbus_Slave_Create
     (Mode      : C_Protocol_Mode;
      Unit_Id   : unsigned_char;
      Callbacks : access Slave_Callbacks) return C_Slave_Handle
     with Export, Convention => C, External_Name => "modbus_slave_create";

   --  Destroy a slave context
   procedure Modbus_Slave_Destroy (Handle : C_Slave_Handle)
     with Export, Convention => C, External_Name => "modbus_slave_destroy";

   --  Process a received request and generate response
   --  Returns: length of response (0 if no response needed, e.g. broadcast)
   --           -1 on error
   function Modbus_Slave_Process
     (Handle          : C_Slave_Handle;
      Request         : access unsigned_char;
      Request_Length  : int;
      Response        : access unsigned_char;
      Response_Size   : int) return int
     with Export, Convention => C, External_Name => "modbus_slave_process";

   --  I/O helpers for slave server loop
   function Modbus_TCP_Receive_Frame
     (Handle      : C_TCP_Handle;
      Buffer      : access unsigned_char;
      Buffer_Size : int;
      Timeout_Ms  : int;
      Length      : access int) return C_Status
     with Export, Convention => C, External_Name => "modbus_tcp_receive_frame";

   function Modbus_TCP_Send_Frame
     (Handle : C_TCP_Handle;
      Frame  : access unsigned_char;
      Length : int) return C_Status
     with Export, Convention => C, External_Name => "modbus_tcp_send_frame";

   ---------------------
   --  Utility Functions
   ---------------------

   --  Convert status code to string
   function Modbus_Status_String
     (Status : C_Status) return Interfaces.C.Strings.chars_ptr
     with Export, Convention => C, External_Name => "modbus_status_string";

   --  Library version
   function Modbus_Version return Interfaces.C.Strings.chars_ptr
     with Export, Convention => C, External_Name => "modbus_version";

end Ada_Modbus.C_API;
