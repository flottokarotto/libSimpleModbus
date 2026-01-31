--  Main TLS Server - Embedded Modbus TLS Test
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  This example demonstrates a Modbus TLS server (slave) running on
--  a Cortex-M MCU. It listens for TLS connections and responds to
--  Read Holding Registers requests with test data.
--
--  For testing:
--    1. Run QEMU with networking:
--       qemu-system-arm -M lm3s6965evb -nographic -semihosting \
--         -netdev user,id=net0,hostfwd=tcp::8802-:802 \
--         -net nic,netdev=net0 \
--         -kernel bin/main_tls_server.elf
--    2. Connect with a TLS client to localhost:8802

with Ada_Modbus;                        use Ada_Modbus;
with Ada_Modbus.Protocol;               use Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.TCP;           use Ada_Modbus.Protocol.TCP;
with Ada_Modbus.Transport.TLS_Mbed;     use Ada_Modbus.Transport.TLS_Mbed;
with Test_Certificates;

--  For semihosting output
with Semihosting;

procedure Main_TLS_Server is

   Server     : TLS_Server;
   Connection : TLS_Connection;
   Config     : TLS_Config;
   Result     : Status;

   --  Buffers for Modbus communication
   Request_Buffer  : Byte_Array (0 .. Max_ADU_Size - 1);
   Request_Length  : Natural;
   Response_Buffer : Byte_Array (0 .. Max_ADU_Size - 1);
   Response_Length : Natural;

   --  Simulated holding registers (addresses 0-99)
   Holding_Registers : Register_Array (0 .. 99) := [others => 0];

   procedure Put_Line (S : String) renames Semihosting.Put_Line;

   --  Process a Read Holding Registers request
   procedure Handle_Read_Registers
     (Request  : Byte_Array;
      Req_Len  : Natural;
      Response : out Byte_Array;
      Resp_Len : out Natural)
   is
      --  MBAP header fields from request
      Trans_Hi  : constant Byte := Request (0);
      Trans_Lo  : constant Byte := Request (1);
      Unit      : constant Byte := Request (6);
      --  PDU fields
      Start_Addr : Register_Address;
      Quantity   : Natural;
      Byte_Count : Natural;
   begin
      pragma Unreferenced (Req_Len);

      --  Parse start address and quantity from request PDU
      Start_Addr := Register_Address (Natural (Request (8)) * 256 +
                                      Natural (Request (9)));
      Quantity := Natural (Request (10)) * 256 + Natural (Request (11));

      --  Validate
      if Quantity < 1 or Quantity > 125 or
         Natural (Start_Addr) + Quantity > Holding_Registers'Length
      then
         --  Build exception response
         Response (0) := Trans_Hi;
         Response (1) := Trans_Lo;
         Response (2) := 0;
         Response (3) := 0;
         Response (4) := 0;
         Response (5) := 3;  --  Length: Unit ID + FC + Exception Code
         Response (6) := Unit;
         Response (7) := Byte (FC_Read_Holding_Registers) + 16#80#;  --  Error FC
         Response (8) := 2;  --  Illegal Data Address
         Resp_Len := 9;
         return;
      end if;

      Byte_Count := Quantity * 2;

      --  Build response MBAP header
      Response (0) := Trans_Hi;
      Response (1) := Trans_Lo;
      Response (2) := 0;  --  Protocol ID
      Response (3) := 0;
      --  Length = Unit ID (1) + FC (1) + Byte Count (1) + Data (Byte_Count)
      Response (4) := Byte (((Byte_Count + 3) / 256) mod 256);
      Response (5) := Byte ((Byte_Count + 3) mod 256);
      Response (6) := Unit;

      --  Build response PDU
      Response (7) := Byte (FC_Read_Holding_Registers);
      Response (8) := Byte (Byte_Count);

      --  Copy register values (big-endian)
      for I in 0 .. Quantity - 1 loop
         declare
            Reg_Val : constant Register_Value :=
               Holding_Registers (Natural (Start_Addr) + I);
            Idx : constant Natural := 9 + I * 2;
         begin
            Response (Idx)     := Byte (Reg_Val / 256);
            Response (Idx + 1) := Byte (Reg_Val mod 256);
         end;
      end loop;

      Resp_Len := 9 + Byte_Count;
   end Handle_Read_Registers;

begin
   Put_Line ("=== Modbus TLS Server Test ===");
   Put_Line ("");

   --  Initialize test data in holding registers
   for I in Holding_Registers'Range loop
      Holding_Registers (I) := Register_Value (I * 100);
   end loop;

   --  Configure TLS with server certificates (certificate mode)
   Config := (
      Mode            => Auth_Certificate,
      Certificate     => Test_Certificates.Server_Cert'Address,
      Certificate_Len => Test_Certificates.Server_Cert'Length,
      Private_Key     => Test_Certificates.Server_Key'Address,
      Private_Key_Len => Test_Certificates.Server_Key'Length,
      CA_Certificate  => Test_Certificates.CA_Cert'Address,
      CA_Cert_Len     => Test_Certificates.CA_Cert'Length,
      Verify_Peer     => False,  --  Don't require client certificates
      others          => <>
   );

   Put_Line ("Starting TLS server on port 802...");

   --  Start listening
   Listen (Server, 802, Config, Result);

   if Result /= Success then
      Put_Line ("ERROR: Failed to start server");
      Put_Line ("TLS Server Test FAILED");
      loop null; end loop;
   end if;

   Put_Line ("Server listening, waiting for connection...");

   --  Main server loop
   loop
      --  Accept a client connection
      Accept_Connection (Server, Connection, 30000, Result);

      if Result = Success then
         Put_Line ("Client connected");

         --  Handle requests from this client
         Client_Loop :
         loop
            --  Receive request
            Receive_Frame (Connection, Request_Buffer, Request_Length,
                           5000, Result);

            exit Client_Loop when Result /= Success;

            Put_Line ("Received request");

            --  Check function code
            if Request_Length >= 8 then
               declare
                  FC : constant Byte := Request_Buffer (7);
               begin
                  if FC = Byte (FC_Read_Holding_Registers) then
                     Handle_Read_Registers
                       (Request_Buffer, Request_Length,
                        Response_Buffer, Response_Length);

                     --  Send response
                     Send_Frame (Connection,
                                 Response_Buffer (0 .. Response_Length - 1),
                                 Result);

                     if Result = Success then
                        Put_Line ("Response sent");
                     else
                        Put_Line ("Send failed");
                        exit Client_Loop;
                     end if;
                  else
                     Put_Line ("Unsupported function code");
                     --  Build exception response (Illegal Function)
                     Response_Buffer (0 .. 5) := Request_Buffer (0 .. 5);
                     Response_Buffer (5) := 3;  --  Length
                     Response_Buffer (6) := Request_Buffer (6);  --  Unit ID
                     Response_Buffer (7) := FC + 16#80#;
                     Response_Buffer (8) := 1;  --  Illegal Function
                     Send_Frame (Connection, Response_Buffer (0 .. 8), Result);
                  end if;
               end;
            end if;
         end loop Client_Loop;

         Put_Line ("Client disconnected");
         Disconnect (Connection);

      elsif Result = Timeout then
         --  No client connected within timeout, continue waiting
         null;
      else
         Put_Line ("Accept error");
      end if;
   end loop;

end Main_TLS_Server;
