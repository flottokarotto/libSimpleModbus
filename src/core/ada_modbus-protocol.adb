--  Ada_Modbus.Protocol - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada_Modbus.Utilities; use Ada_Modbus.Utilities;

package body Ada_Modbus.Protocol
  with SPARK_Mode => On
is

   --  Bit mask lookup table to avoid exponentiation
   Bit_Masks : constant array (Natural range 0 .. 7) of Byte :=
     [1, 2, 4, 8, 16, 32, 64, 128];

   -----------------------
   -- To_Exception_Byte --
   -----------------------

   function To_Exception_Byte (S : Status) return Byte is
   begin
      case S is
         when Exception_Illegal_Function => return 16#01#;
         when Exception_Illegal_Address  => return 16#02#;
         when Exception_Illegal_Value    => return 16#03#;
         when Exception_Slave_Failure    => return 16#04#;
         when Exception_Acknowledge      => return 16#05#;
         when Exception_Slave_Busy       => return 16#06#;
         when Exception_Gateway_Path     => return 16#0A#;
         when Exception_Gateway_Target   => return 16#0B#;
         when others                     => return 16#01#;  --  Should not happen
      end case;
   end To_Exception_Byte;

   -------------------------
   -- From_Exception_Byte --
   -------------------------

   function From_Exception_Byte (Code : Byte) return Status is
   begin
      case Code is
         when 16#01# => return Exception_Illegal_Function;
         when 16#02# => return Exception_Illegal_Address;
         when 16#03# => return Exception_Illegal_Value;
         when 16#04# => return Exception_Slave_Failure;
         when 16#05# => return Exception_Acknowledge;
         when 16#06# => return Exception_Slave_Busy;
         when 16#0A# => return Exception_Gateway_Path;
         when 16#0B# => return Exception_Gateway_Target;
         when others => return Exception_Illegal_Function;
      end case;
   end From_Exception_Byte;

   -------------------------------
   -- Encode_Read_Bits_Request --
   -------------------------------

   procedure Encode_Read_Bits_Request
     (Buffer        : out PDU_Buffer;
      Length        : out Natural;
      FC            : Function_Code;
      Start_Address : Coil_Address;
      Quantity      : Coil_Count)
   is
      Addr : constant Byte_Array := To_Big_Endian (Register_Value (Start_Address));
      Qty  : constant Byte_Array := To_Big_Endian (Register_Value (Quantity));
   begin
      Buffer := [others => 0];
      Buffer (0) := Byte (FC);
      Buffer (1) := Addr (0);
      Buffer (2) := Addr (1);
      Buffer (3) := Qty (0);
      Buffer (4) := Qty (1);
      Length := 5;
   end Encode_Read_Bits_Request;

   -----------------------------------
   -- Encode_Read_Registers_Request --
   -----------------------------------

   procedure Encode_Read_Registers_Request
     (Buffer        : out PDU_Buffer;
      Length        : out Natural;
      FC            : Function_Code;
      Start_Address : Register_Address;
      Quantity      : Register_Count)
   is
      Addr : constant Byte_Array := To_Big_Endian (Register_Value (Start_Address));
      Qty  : constant Byte_Array := To_Big_Endian (Register_Value (Quantity));
   begin
      Buffer := [others => 0];
      Buffer (0) := Byte (FC);
      Buffer (1) := Addr (0);
      Buffer (2) := Addr (1);
      Buffer (3) := Qty (0);
      Buffer (4) := Qty (1);
      Length := 5;
   end Encode_Read_Registers_Request;

   -------------------------------------
   -- Encode_Write_Single_Coil_Request --
   -------------------------------------

   procedure Encode_Write_Single_Coil_Request
     (Buffer  : out PDU_Buffer;
      Length  : out Natural;
      Address : Coil_Address;
      Value   : Coil_Value)
   is
      Addr     : constant Byte_Array := To_Big_Endian (Register_Value (Address));
      Coil_Val : constant Register_Value := (if Value then 16#FF00# else 16#0000#);
      Val      : constant Byte_Array := To_Big_Endian (Coil_Val);
   begin
      Buffer := [others => 0];
      Buffer (0) := Byte (FC_Write_Single_Coil);
      Buffer (1) := Addr (0);
      Buffer (2) := Addr (1);
      Buffer (3) := Val (0);
      Buffer (4) := Val (1);
      Length := 5;
   end Encode_Write_Single_Coil_Request;

   -----------------------------------------
   -- Encode_Write_Single_Register_Request --
   -----------------------------------------

   procedure Encode_Write_Single_Register_Request
     (Buffer  : out PDU_Buffer;
      Length  : out Natural;
      Address : Register_Address;
      Value   : Register_Value)
   is
      Addr : constant Byte_Array := To_Big_Endian (Register_Value (Address));
      Val  : constant Byte_Array := To_Big_Endian (Value);
   begin
      Buffer := [others => 0];
      Buffer (0) := Byte (FC_Write_Single_Register);
      Buffer (1) := Addr (0);
      Buffer (2) := Addr (1);
      Buffer (3) := Val (0);
      Buffer (4) := Val (1);
      Length := 5;
   end Encode_Write_Single_Register_Request;

   ----------------------------------------
   -- Encode_Write_Multiple_Coils_Request --
   ----------------------------------------

   procedure Encode_Write_Multiple_Coils_Request
     (Buffer        : out PDU_Buffer;
      Length        : out Natural;
      Start_Address : Coil_Address;
      Values        : Coil_Array)
   is
      Addr       : constant Byte_Array := To_Big_Endian (Register_Value (Start_Address));
      Qty        : constant Natural := Values'Length;
      Qty_Bytes  : constant Byte_Array := To_Big_Endian (Register_Value (Qty));
      Byte_Count : constant Natural := (Qty + 7) / 8;
      Bit_Idx    : Natural := 0;
      Byte_Val   : Byte := 0;
   begin
      Buffer := [others => 0];
      Buffer (0) := Byte (FC_Write_Multiple_Coils);
      Buffer (1) := Addr (0);
      Buffer (2) := Addr (1);
      Buffer (3) := Qty_Bytes (0);
      Buffer (4) := Qty_Bytes (1);
      Buffer (5) := Byte (Byte_Count);

      --  Pack coils into bytes (LSB first within each byte)
      for I in Values'Range loop
         pragma Loop_Invariant (Bit_Idx <= 7);
         pragma Loop_Invariant (6 + (I - Values'First) / 8 < Max_PDU_Size);
         if Values (I) then
            Byte_Val := Byte_Val or Bit_Masks (Bit_Idx);
         end if;
         Bit_Idx := Bit_Idx + 1;
         if Bit_Idx = 8 then
            Buffer (6 + (I - Values'First) / 8) := Byte_Val;
            Byte_Val := 0;
            Bit_Idx := 0;
         end if;
      end loop;

      --  Store remaining bits
      if Bit_Idx > 0 then
         Buffer (6 + Byte_Count - 1) := Byte_Val;
      end if;

      Length := 6 + Byte_Count;
   end Encode_Write_Multiple_Coils_Request;

   --------------------------------------------
   -- Encode_Write_Multiple_Registers_Request --
   --------------------------------------------

   procedure Encode_Write_Multiple_Registers_Request
     (Buffer        : out PDU_Buffer;
      Length        : out Natural;
      Start_Address : Register_Address;
      Values        : Register_Array)
   is
      Addr       : constant Byte_Array := To_Big_Endian (Register_Value (Start_Address));
      Qty        : constant Natural := Values'Length;
      Qty_Bytes  : constant Byte_Array := To_Big_Endian (Register_Value (Qty));
      Byte_Count : constant Natural := Qty * 2;
      Idx        : Natural := 6;
   begin
      Buffer := [others => 0];
      Buffer (0) := Byte (FC_Write_Multiple_Registers);
      Buffer (1) := Addr (0);
      Buffer (2) := Addr (1);
      Buffer (3) := Qty_Bytes (0);
      Buffer (4) := Qty_Bytes (1);
      Buffer (5) := Byte (Byte_Count);

      for I in Values'Range loop
         pragma Loop_Invariant (Idx = 6 + 2 * (I - Values'First));
         pragma Loop_Invariant (Idx + 1 < Max_PDU_Size);
         declare
            Val : constant Byte_Array := To_Big_Endian (Values (I));
         begin
            Buffer (Idx) := Val (0);
            Buffer (Idx + 1) := Val (1);
            Idx := Idx + 2;
         end;
      end loop;

      Length := 6 + Byte_Count;
   end Encode_Write_Multiple_Registers_Request;

   -------------------------------
   -- Decode_Read_Bits_Response --
   -------------------------------

   procedure Decode_Read_Bits_Response
     (Buffer   : PDU_Buffer;
      Length : PDU_Data_Length;
      Values   : out Coil_Array;
      Count    : out Natural;
      Response : out Status)
   is
      FC : constant Function_Code := Function_Code (Buffer (0));
   begin
      Values := [others => False];
      Count := 0;

      if Is_Exception_Response (FC) then
         Response := From_Exception_Byte (Buffer (1));
         return;
      end if;

      if Length < 2 then
         Response := Frame_Error;
         return;
      end if;

      declare
         Byte_Count : constant Natural := Natural (Buffer (1));
         Bit_Count  : Natural;
      begin
         --  Validate Byte_Count bounds (max 250 data bytes in PDU)
         if Byte_Count > Max_PDU_Size - 2 then
            Response := Frame_Error;
            return;
         end if;

         if Length < 2 + Byte_Count then
            Response := Frame_Error;
            return;
         end if;

         Bit_Count := Natural'Min (Byte_Count * 8, Values'Length);

         for I in 0 .. Bit_Count - 1 loop
            pragma Loop_Invariant (Values'First + I <= Values'Last);
            declare
               Byte_Idx : constant Natural := I / 8;
               Bit_Idx  : constant Natural := I mod 8;
               B        : constant Byte := Buffer (2 + Byte_Idx);
            begin
               pragma Assert (Bit_Idx <= 7);
               Values (Values'First + I) := (B and Bit_Masks (Bit_Idx)) /= 0;
            end;
         end loop;
         Count := Bit_Count;
      end;

      Response := Success;
   end Decode_Read_Bits_Response;

   -----------------------------------
   -- Decode_Read_Registers_Response --
   -----------------------------------

   procedure Decode_Read_Registers_Response
     (Buffer   : PDU_Buffer;
      Length : PDU_Data_Length;
      Values   : out Register_Array;
      Count    : out Natural;
      Response : out Status)
   is
      FC : constant Function_Code := Function_Code (Buffer (0));
   begin
      Values := [others => 0];
      Count := 0;

      if Is_Exception_Response (FC) then
         Response := From_Exception_Byte (Buffer (1));
         return;
      end if;

      if Length < 2 then
         Response := Frame_Error;
         return;
      end if;

      declare
         Byte_Count : constant Natural := Natural (Buffer (1));
         Reg_Count  : Natural;
      begin
         --  Validate Byte_Count bounds (max 250 data bytes in PDU)
         if Byte_Count > Max_PDU_Size - 2 then
            Response := Frame_Error;
            return;
         end if;

         if Length < 2 + Byte_Count then
            Response := Frame_Error;
            return;
         end if;

         Reg_Count := Byte_Count / 2;

         for I in 0 .. Natural'Min (Reg_Count, Values'Length) - 1 loop
            pragma Loop_Invariant (I < Natural'Min (Reg_Count, Values'Length));
            pragma Loop_Invariant (Values'First + I <= Values'Last);
            pragma Loop_Invariant (3 + I * 2 < Max_PDU_Size);
            Values (Values'First + I) := From_Big_Endian
              (Buffer (2 + I * 2), Buffer (3 + I * 2));
         end loop;
         Count := Natural'Min (Reg_Count, Values'Length);
      end;

      Response := Success;
   end Decode_Read_Registers_Response;

   ---------------------------------
   -- Decode_Write_Single_Response --
   ---------------------------------

   procedure Decode_Write_Single_Response
     (Buffer   : PDU_Buffer;
      Length : PDU_Data_Length;
      Address  : out Register_Address;
      Value    : out Register_Value;
      Response : out Status)
   is
      FC : constant Function_Code := Function_Code (Buffer (0));
   begin
      Address := 0;
      Value := 0;

      if Is_Exception_Response (FC) then
         Response := From_Exception_Byte (Buffer (1));
         return;
      end if;

      if Length < 5 then
         Response := Frame_Error;
         return;
      end if;

      Address := Register_Address (From_Big_Endian (Buffer (1), Buffer (2)));
      Value := From_Big_Endian (Buffer (3), Buffer (4));
      Response := Success;
   end Decode_Write_Single_Response;

   -----------------------------------
   -- Decode_Write_Multiple_Response --
   -----------------------------------

   procedure Decode_Write_Multiple_Response
     (Buffer        : PDU_Buffer;
      Length : PDU_Data_Length;
      Start_Address : out Register_Address;
      Quantity      : out Natural;
      Response      : out Status)
   is
      FC : constant Function_Code := Function_Code (Buffer (0));
   begin
      Start_Address := 0;
      Quantity := 0;

      if Is_Exception_Response (FC) then
         Response := From_Exception_Byte (Buffer (1));
         return;
      end if;

      if Length < 5 then
         Response := Frame_Error;
         return;
      end if;

      Start_Address := Register_Address (From_Big_Endian (Buffer (1), Buffer (2)));
      Quantity := Natural (From_Big_Endian (Buffer (3), Buffer (4)));
      Response := Success;
   end Decode_Write_Multiple_Response;

   --------------------------------
   -- Decode_Exception_Response --
   --------------------------------

   procedure Decode_Exception_Response
     (Buffer   : PDU_Buffer;
      Length : PDU_Data_Length;
      Response : out Status)
   is
   begin
      if Length < 2 then
         Response := Frame_Error;
         return;
      end if;

      Response := From_Exception_Byte (Buffer (1));
   end Decode_Exception_Response;

   ------------------------------------------
   -- Encode_Read_Exception_Status_Request --
   ------------------------------------------

   procedure Encode_Read_Exception_Status_Request
     (Buffer : out PDU_Buffer;
      Length : out Natural)
   is
   begin
      Buffer := [others => 0];
      Buffer (0) := Byte (FC_Read_Exception_Status);
      Length := 1;
   end Encode_Read_Exception_Status_Request;

   -------------------------------------------
   -- Decode_Read_Exception_Status_Response --
   -------------------------------------------

   procedure Decode_Read_Exception_Status_Response
     (Buffer           : PDU_Buffer;
      Length : PDU_Data_Length;
      Exception_Status : out Byte;
      Response         : out Status)
   is
      FC : constant Function_Code := Function_Code (Buffer (0));
   begin
      Exception_Status := 0;

      if Is_Exception_Response (FC) then
         Response := From_Exception_Byte (Buffer (1));
         return;
      end if;

      if Length < 2 then
         Response := Frame_Error;
         return;
      end if;

      Exception_Status := Buffer (1);
      Response := Success;
   end Decode_Read_Exception_Status_Response;

   --------------------------------
   -- Encode_Diagnostics_Request --
   --------------------------------

   procedure Encode_Diagnostics_Request
     (Buffer       : out PDU_Buffer;
      Length       : out Natural;
      Sub_Function : Interfaces.Unsigned_16;
      Data         : Interfaces.Unsigned_16)
   is
      Sub  : constant Byte_Array := To_Big_Endian (Register_Value (Sub_Function));
      Dat  : constant Byte_Array := To_Big_Endian (Register_Value (Data));
   begin
      Buffer := [others => 0];
      Buffer (0) := Byte (FC_Diagnostics);
      Buffer (1) := Sub (0);
      Buffer (2) := Sub (1);
      Buffer (3) := Dat (0);
      Buffer (4) := Dat (1);
      Length := 5;
   end Encode_Diagnostics_Request;

   ---------------------------------
   -- Decode_Diagnostics_Response --
   ---------------------------------

   procedure Decode_Diagnostics_Response
     (Buffer       : PDU_Buffer;
      Length : PDU_Data_Length;
      Sub_Function : out Interfaces.Unsigned_16;
      Data         : out Interfaces.Unsigned_16;
      Response     : out Status)
   is
      FC : constant Function_Code := Function_Code (Buffer (0));
   begin
      Sub_Function := 0;
      Data := 0;

      if Is_Exception_Response (FC) then
         Response := From_Exception_Byte (Buffer (1));
         return;
      end if;

      if Length < 5 then
         Response := Frame_Error;
         return;
      end if;

      Sub_Function := Interfaces.Unsigned_16 (From_Big_Endian (Buffer (1), Buffer (2)));
      Data := Interfaces.Unsigned_16 (From_Big_Endian (Buffer (3), Buffer (4)));
      Response := Success;
   end Decode_Diagnostics_Response;

   ------------------------------------
   -- Encode_Report_Server_Id_Request --
   ------------------------------------

   procedure Encode_Report_Server_Id_Request
     (Buffer : out PDU_Buffer;
      Length : out Natural)
   is
   begin
      Buffer := [others => 0];
      Buffer (0) := Byte (FC_Report_Server_Id);
      Length := 1;
   end Encode_Report_Server_Id_Request;

   -------------------------------------
   -- Decode_Report_Server_Id_Response --
   -------------------------------------

   procedure Decode_Report_Server_Id_Response
     (Buffer        : PDU_Buffer;
      Length : PDU_Data_Length;
      Server_Id     : out Byte;
      Run_Indicator : out Boolean;
      Add_Data      : out Byte_Array;
      Add_Data_Len  : out Natural;
      Response      : out Status)
   is
      FC : constant Function_Code := Function_Code (Buffer (0));
   begin
      Server_Id := 0;
      Run_Indicator := False;
      Add_Data := [others => 0];
      Add_Data_Len := 0;

      if Is_Exception_Response (FC) then
         Response := From_Exception_Byte (Buffer (1));
         return;
      end if;

      if Length < 4 then
         Response := Frame_Error;
         return;
      end if;

      declare
         Byte_Count : constant Natural := Natural (Buffer (1));
      begin
         if Length < 2 + Byte_Count or else Byte_Count < 2 then
            Response := Frame_Error;
            return;
         end if;

         Server_Id := Buffer (2);
         Run_Indicator := Buffer (3) = 16#FF#;

         --  Copy additional data if present
         Add_Data_Len := Natural'Min (Byte_Count - 2, Add_Data'Length);
         for I in 0 .. Add_Data_Len - 1 loop
            pragma Loop_Invariant (I < Add_Data_Len);
            pragma Loop_Invariant (Add_Data'First + I <= Add_Data'Last);
            pragma Loop_Invariant (4 + I < Max_PDU_Size);
            Add_Data (Add_Data'First + I) := Buffer (4 + I);
         end loop;
      end;

      Response := Success;
   end Decode_Report_Server_Id_Response;

   --------------------------------------
   -- Encode_Mask_Write_Register_Request --
   --------------------------------------

   procedure Encode_Mask_Write_Register_Request
     (Buffer   : out PDU_Buffer;
      Length   : out Natural;
      Address  : Register_Address;
      And_Mask : Register_Value;
      Or_Mask  : Register_Value)
   is
      Addr    : constant Byte_Array := To_Big_Endian (Register_Value (Address));
      And_Val : constant Byte_Array := To_Big_Endian (And_Mask);
      Or_Val  : constant Byte_Array := To_Big_Endian (Or_Mask);
   begin
      Buffer := [others => 0];
      Buffer (0) := Byte (FC_Mask_Write_Register);
      Buffer (1) := Addr (0);
      Buffer (2) := Addr (1);
      Buffer (3) := And_Val (0);
      Buffer (4) := And_Val (1);
      Buffer (5) := Or_Val (0);
      Buffer (6) := Or_Val (1);
      Length := 7;
   end Encode_Mask_Write_Register_Request;

   ---------------------------------------
   -- Decode_Mask_Write_Register_Response --
   ---------------------------------------

   procedure Decode_Mask_Write_Register_Response
     (Buffer   : PDU_Buffer;
      Length : PDU_Data_Length;
      Address  : out Register_Address;
      And_Mask : out Register_Value;
      Or_Mask  : out Register_Value;
      Response : out Status)
   is
      FC : constant Function_Code := Function_Code (Buffer (0));
   begin
      Address := 0;
      And_Mask := 0;
      Or_Mask := 0;

      if Is_Exception_Response (FC) then
         Response := From_Exception_Byte (Buffer (1));
         return;
      end if;

      if Length < 7 then
         Response := Frame_Error;
         return;
      end if;

      Address := Register_Address (From_Big_Endian (Buffer (1), Buffer (2)));
      And_Mask := From_Big_Endian (Buffer (3), Buffer (4));
      Or_Mask := From_Big_Endian (Buffer (5), Buffer (6));
      Response := Success;
   end Decode_Mask_Write_Register_Response;

   ------------------------------------------
   -- Encode_Read_Write_Registers_Request --
   ------------------------------------------

   procedure Encode_Read_Write_Registers_Request
     (Buffer            : out PDU_Buffer;
      Length            : out Natural;
      Read_Start        : Register_Address;
      Read_Quantity     : Register_Count;
      Write_Start       : Register_Address;
      Write_Values      : Register_Array)
   is
      RStart     : constant Byte_Array := To_Big_Endian (Register_Value (Read_Start));
      RQty       : constant Byte_Array := To_Big_Endian (Register_Value (Read_Quantity));
      WStart     : constant Byte_Array := To_Big_Endian (Register_Value (Write_Start));
      WQty       : constant Byte_Array := To_Big_Endian (Register_Value (Write_Values'Length));
      Byte_Count : constant Natural := Write_Values'Length * 2;
      Idx        : Natural := 10;
   begin
      Buffer := [others => 0];
      Buffer (0) := Byte (FC_Read_Write_Multiple_Registers);
      Buffer (1) := RStart (0);
      Buffer (2) := RStart (1);
      Buffer (3) := RQty (0);
      Buffer (4) := RQty (1);
      Buffer (5) := WStart (0);
      Buffer (6) := WStart (1);
      Buffer (7) := WQty (0);
      Buffer (8) := WQty (1);
      Buffer (9) := Byte (Byte_Count);

      for I in Write_Values'Range loop
         pragma Loop_Invariant (Idx = 10 + 2 * (I - Write_Values'First));
         pragma Loop_Invariant (Idx + 1 < Max_PDU_Size);
         declare
            Val : constant Byte_Array := To_Big_Endian (Write_Values (I));
         begin
            Buffer (Idx) := Val (0);
            Buffer (Idx + 1) := Val (1);
            Idx := Idx + 2;
         end;
      end loop;

      Length := 10 + Byte_Count;
   end Encode_Read_Write_Registers_Request;

   -------------------------------------------
   -- Decode_Read_Write_Registers_Response --
   -------------------------------------------

   procedure Decode_Read_Write_Registers_Response
     (Buffer   : PDU_Buffer;
      Length : PDU_Data_Length;
      Values   : out Register_Array;
      Count    : out Natural;
      Response : out Status)
   is
   begin
      --  Response format is same as Read Holding Registers
      Decode_Read_Registers_Response (Buffer, Length, Values, Count, Response);
   end Decode_Read_Write_Registers_Response;

end Ada_Modbus.Protocol;
