--  Ada_Modbus.Protocol.TCP - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada_Modbus.Utilities; use Ada_Modbus.Utilities;
with Interfaces; use Interfaces;

package body Ada_Modbus.Protocol.TCP
  with SPARK_Mode => On
is

   -----------------
   -- Build_Frame --
   -----------------

   procedure Build_Frame
     (ADU           : out ADU_Buffer;
      ADU_Length    : out Natural;
      Transaction   : Transaction_Id;
      Unit          : Unit_Id;
      PDU           : PDU_Buffer;
      PDU_Length    : PDU_Data_Length)
   is
      Trans_Bytes  : constant Byte_Array := To_Big_Endian (Register_Value (Transaction));
      Proto_Bytes  : constant Byte_Array := To_Big_Endian (Register_Value (Modbus_Protocol_ID));
      Length_Field : constant Natural := PDU_Length + 1;  --  Unit ID + PDU
      Len_Bytes    : constant Byte_Array := To_Big_Endian (Register_Value (Length_Field));
   begin
      ADU := [others => 0];
      --  Transaction ID (bytes 0-1)
      ADU (0) := Trans_Bytes (0);
      ADU (1) := Trans_Bytes (1);

      --  Protocol ID (bytes 2-3)
      ADU (2) := Proto_Bytes (0);
      ADU (3) := Proto_Bytes (1);

      --  Length (bytes 4-5)
      ADU (4) := Len_Bytes (0);
      ADU (5) := Len_Bytes (1);

      --  Unit ID (byte 6)
      ADU (6) := Byte (Unit);

      --  Copy PDU (bytes 7+)
      for I in 0 .. PDU_Length - 1 loop
         ADU (MBAP_Header_Size + I) := PDU (I);
      end loop;

      ADU_Length := MBAP_Header_Size + PDU_Length;
   end Build_Frame;

   -----------------
   -- Parse_Frame --
   -----------------

   procedure Parse_Frame
     (ADU           : ADU_Buffer;
      ADU_Length    : ADU_Data_Length;
      Transaction   : out Transaction_Id;
      Unit          : out Unit_Id;
      PDU           : out PDU_Buffer;
      PDU_Length    : out Natural;
      Result        : out Status)
   is
      Proto_ID     : Unsigned_16;
      Length_Field : Natural;
   begin
      --  Initialize outputs
      Transaction := 0;
      Unit := 0;
      PDU := [others => 0];
      PDU_Length := 0;

      --  Minimum frame: 7 MBAP + 1 FC = 8 bytes
      if ADU_Length < 8 then
         Result := Frame_Error;
         return;
      end if;

      --  Extract and verify protocol ID
      Proto_ID := Unsigned_16 (From_Big_Endian (ADU (2), ADU (3)));
      if Proto_ID /= Modbus_Protocol_ID then
         Result := Frame_Error;
         return;
      end if;

      --  Extract length field
      Length_Field := Natural (From_Big_Endian (ADU (4), ADU (5)));

      --  Validate length field bounds (max PDU + Unit ID = 254)
      if Length_Field < 2 or else Length_Field > Max_PDU_Size + 1 then
         Result := Frame_Error;
         return;
      end if;

      --  Verify length consistency
      if ADU_Length /= MBAP_Header_Size - 1 + Length_Field then
         Result := Frame_Error;
         return;
      end if;

      --  Extract transaction ID
      Transaction := Transaction_Id (From_Big_Endian (ADU (0), ADU (1)));

      --  Extract unit ID (validate range for Unit_Id)
      if ADU (6) > 247 then
         Result := Frame_Error;
         return;
      end if;
      Unit := Unit_Id (ADU (6));

      --  Extract PDU
      PDU_Length := Length_Field - 1;  --  Subtract Unit ID
      for I in 0 .. PDU_Length - 1 loop
         pragma Loop_Invariant (MBAP_Header_Size + I < ADU_Length);
         pragma Loop_Invariant (I < PDU_Length);
         PDU (I) := ADU (MBAP_Header_Size + I);
      end loop;

      Result := Success;
   end Parse_Frame;

   ------------------------
   -- Get_Transaction_Id --
   ------------------------

   function Get_Transaction_Id (ADU : ADU_Buffer) return Transaction_Id is
   begin
      return Transaction_Id (From_Big_Endian (ADU (0), ADU (1)));
   end Get_Transaction_Id;

   -------------------------
   -- Get_Expected_Length --
   -------------------------

   function Get_Expected_Length (ADU : ADU_Buffer) return Natural is
      Length_Field : constant Natural := Natural (From_Big_Endian (ADU (4), ADU (5)));
   begin
      --  Total frame = 6 bytes MBAP (before length) + length field value
      return 6 + Length_Field;
   end Get_Expected_Length;

end Ada_Modbus.Protocol.TCP;
