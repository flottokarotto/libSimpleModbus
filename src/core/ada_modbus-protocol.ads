--  Ada_Modbus.Protocol - PDU Encoding/Decoding
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  PDU (Protocol Data Unit) is the core of Modbus protocol:
--  - Function Code (1 byte)
--  - Data (variable length, max 252 bytes)
--
--  ADU (Application Data Unit) = PDU + framing (address, CRC/LRC/MBAP)

package Ada_Modbus.Protocol
  with SPARK_Mode => On
is

   pragma Pure;

   --  Maximum PDU size per Modbus specification
   Max_PDU_Size : constant := 253;  --  1 FC + 252 data

   --  PDU buffer type
   subtype PDU_Index is Natural range 0 .. Max_PDU_Size - 1;
   subtype PDU_Buffer is Byte_Array (0 .. Max_PDU_Size - 1);

   --  Exception code in response (function code + 0x80)
   function Is_Exception_Response (FC : Function_Code) return Boolean is
     (FC >= 16#80#);

   function Get_Exception_Code (FC : Function_Code) return Byte is
     (Byte (FC and 16#7F#))
     with Pre => Is_Exception_Response (FC);

   --  Convert Status to Modbus exception code byte
   function To_Exception_Byte (S : Status) return Byte
     with Pre => S in Exception_Illegal_Function .. Exception_Gateway_Target;

   --  Convert Modbus exception code byte to Status
   function From_Exception_Byte (Code : Byte) return Status;

   -------------------
   --  Request PDUs --
   -------------------

   --  Read Coils (FC 01) / Read Discrete Inputs (FC 02)
   --  Request: FC(1) + Start Address(2) + Quantity(2) = 5 bytes
   procedure Encode_Read_Bits_Request
     (Buffer        : out PDU_Buffer;
      Length        : out Natural;
      FC            : Function_Code;
      Start_Address : Coil_Address;
      Quantity      : Coil_Count);

   --  Read Holding Registers (FC 03) / Read Input Registers (FC 04)
   --  Request: FC(1) + Start Address(2) + Quantity(2) = 5 bytes
   procedure Encode_Read_Registers_Request
     (Buffer        : out PDU_Buffer;
      Length        : out Natural;
      FC            : Function_Code;
      Start_Address : Register_Address;
      Quantity      : Register_Count);

   --  Write Single Coil (FC 05)
   --  Request: FC(1) + Address(2) + Value(2) = 5 bytes
   --  Value: 0xFF00 = ON, 0x0000 = OFF
   procedure Encode_Write_Single_Coil_Request
     (Buffer  : out PDU_Buffer;
      Length  : out Natural;
      Address : Coil_Address;
      Value   : Coil_Value);

   --  Write Single Register (FC 06)
   --  Request: FC(1) + Address(2) + Value(2) = 5 bytes
   procedure Encode_Write_Single_Register_Request
     (Buffer  : out PDU_Buffer;
      Length  : out Natural;
      Address : Register_Address;
      Value   : Register_Value);

   --  Write Multiple Coils (FC 15)
   --  Request: FC(1) + Start(2) + Quantity(2) + ByteCount(1) + Values(n)
   --  Max coils: 1968 (Modbus spec), PDU constraint: 6 + ceil(n/8) <= 253
   procedure Encode_Write_Multiple_Coils_Request
     (Buffer        : out PDU_Buffer;
      Length        : out Natural;
      Start_Address : Coil_Address;
      Values        : Coil_Array)
     with Pre => Values'Length >= 1
                 and then Values'Length <= 1968;

   --  Write Multiple Registers (FC 16)
   --  Request: FC(1) + Start(2) + Quantity(2) + ByteCount(1) + Values(2*n)
   --  Max registers: 123 (Modbus spec), PDU constraint: 6 + 2*n <= 253
   procedure Encode_Write_Multiple_Registers_Request
     (Buffer        : out PDU_Buffer;
      Length        : out Natural;
      Start_Address : Register_Address;
      Values        : Register_Array)
     with Pre => Values'Length >= 1
                 and then Values'Length <= 123;

   --------------------
   --  Response PDUs --
   --------------------

   --  Read Coils / Read Discrete Inputs Response
   --  Response: FC(1) + ByteCount(1) + CoilStatus(n)
   procedure Decode_Read_Bits_Response
     (Buffer   : PDU_Buffer;
      Length   : Natural;
      Values   : out Coil_Array;
      Count    : out Natural;
      Response : out Status)
     with Pre => Length <= Max_PDU_Size
                 and then Values'Length <= 2000;

   --  Read Holding Registers / Read Input Registers Response
   --  Response: FC(1) + ByteCount(1) + RegisterValues(2*n)
   procedure Decode_Read_Registers_Response
     (Buffer   : PDU_Buffer;
      Length   : Natural;
      Values   : out Register_Array;
      Count    : out Natural;
      Response : out Status)
     with Pre => Length <= Max_PDU_Size
                 and then Values'Length <= 125;

   --  Write Single Coil / Write Single Register Response (echo)
   --  Response: FC(1) + Address(2) + Value(2) = 5 bytes
   procedure Decode_Write_Single_Response
     (Buffer   : PDU_Buffer;
      Length   : Natural;
      Address  : out Register_Address;
      Value    : out Register_Value;
      Response : out Status)
     with Pre => Length <= Max_PDU_Size;

   --  Write Multiple Response
   --  Response: FC(1) + Start(2) + Quantity(2) = 5 bytes
   procedure Decode_Write_Multiple_Response
     (Buffer        : PDU_Buffer;
      Length        : Natural;
      Start_Address : out Register_Address;
      Quantity      : out Natural;
      Response      : out Status)
     with Pre => Length <= Max_PDU_Size;

   --  Exception Response
   --  Response: FC+0x80(1) + ExceptionCode(1) = 2 bytes
   procedure Decode_Exception_Response
     (Buffer   : PDU_Buffer;
      Length   : Natural;
      Response : out Status)
     with Pre => Length <= Max_PDU_Size;

   -----------------------------------
   --  Diagnostic Function Codes   --
   -----------------------------------

   --  Read Exception Status (FC 07)
   --  Request: FC(1) = 1 byte (just function code)
   procedure Encode_Read_Exception_Status_Request
     (Buffer : out PDU_Buffer;
      Length : out Natural);

   --  Read Exception Status Response
   --  Response: FC(1) + ExceptionStatus(1) = 2 bytes
   procedure Decode_Read_Exception_Status_Response
     (Buffer           : PDU_Buffer;
      Length           : Natural;
      Exception_Status : out Byte;
      Response         : out Status)
     with Pre => Length <= Max_PDU_Size;

   --  Diagnostics (FC 08)
   --  Request: FC(1) + SubFunction(2) + Data(2) = 5 bytes
   procedure Encode_Diagnostics_Request
     (Buffer       : out PDU_Buffer;
      Length       : out Natural;
      Sub_Function : Interfaces.Unsigned_16;
      Data         : Interfaces.Unsigned_16);

   --  Diagnostics Response
   --  Response: FC(1) + SubFunction(2) + Data(2) = 5 bytes
   procedure Decode_Diagnostics_Response
     (Buffer       : PDU_Buffer;
      Length       : Natural;
      Sub_Function : out Interfaces.Unsigned_16;
      Data         : out Interfaces.Unsigned_16;
      Response     : out Status)
     with Pre => Length <= Max_PDU_Size;

   --  Report Server ID (FC 17)
   --  Request: FC(1) = 1 byte
   procedure Encode_Report_Server_Id_Request
     (Buffer : out PDU_Buffer;
      Length : out Natural);

   --  Report Server ID Response
   --  Response: FC(1) + ByteCount(1) + ServerID(1) + RunIndicator(1) + Data(n)
   procedure Decode_Report_Server_Id_Response
     (Buffer        : PDU_Buffer;
      Length        : Natural;
      Server_Id     : out Byte;
      Run_Indicator : out Boolean;
      Add_Data      : out Byte_Array;
      Add_Data_Len  : out Natural;
      Response      : out Status)
     with Pre => Length <= Max_PDU_Size
                 and then Add_Data'Length <= Max_PDU_Size;

   --  Mask Write Register (FC 22)
   --  Request: FC(1) + Address(2) + And_Mask(2) + Or_Mask(2) = 7 bytes
   --  Result = (Current AND And_Mask) OR (Or_Mask AND (NOT And_Mask))
   procedure Encode_Mask_Write_Register_Request
     (Buffer   : out PDU_Buffer;
      Length   : out Natural;
      Address  : Register_Address;
      And_Mask : Register_Value;
      Or_Mask  : Register_Value);

   --  Mask Write Register Response (echo)
   procedure Decode_Mask_Write_Register_Response
     (Buffer   : PDU_Buffer;
      Length   : Natural;
      Address  : out Register_Address;
      And_Mask : out Register_Value;
      Or_Mask  : out Register_Value;
      Response : out Status)
     with Pre => Length <= Max_PDU_Size;

   --  Read/Write Multiple Registers (FC 23)
   --  Request: FC(1) + ReadStart(2) + ReadQty(2) + WriteStart(2) + WriteQty(2)
   --           + WriteByteCount(1) + WriteValues(2*n)
   --  Max write registers: 121 (Modbus spec), PDU constraint: 10 + 2*n <= 253
   procedure Encode_Read_Write_Registers_Request
     (Buffer            : out PDU_Buffer;
      Length            : out Natural;
      Read_Start        : Register_Address;
      Read_Quantity     : Register_Count;
      Write_Start       : Register_Address;
      Write_Values      : Register_Array)
     with Pre => Write_Values'Length >= 1
                 and then Write_Values'Length <= 121;

   --  Read/Write Multiple Registers Response
   --  Response: FC(1) + ByteCount(1) + ReadValues(2*n)
   procedure Decode_Read_Write_Registers_Response
     (Buffer   : PDU_Buffer;
      Length   : Natural;
      Values   : out Register_Array;
      Count    : out Natural;
      Response : out Status)
     with Pre => Length <= Max_PDU_Size
                 and then Values'Length <= 125;

end Ada_Modbus.Protocol;
