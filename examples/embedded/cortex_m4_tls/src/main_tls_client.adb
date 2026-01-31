--  Main TLS Client - Embedded Modbus TLS Test
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  This example demonstrates a Modbus TLS client running on
--  a Cortex-M MCU. It connects to a TLS server (via QEMU networking)
--  and performs a simple Read Holding Registers request.
--
--  For CI testing with QEMU:
--    1. Start the TLS test server on the host
--    2. Run QEMU with networking:
--       qemu-system-arm -M lm3s6965evb -nographic -semihosting \
--         -netdev user,id=net0 -net nic,netdev=net0 \
--         -kernel bin/main_tls_client.elf

with Ada_Modbus;                        use Ada_Modbus;
with Ada_Modbus.Protocol;               use Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.TCP;           use Ada_Modbus.Protocol.TCP;
with Ada_Modbus.Transport.TLS_Mbed;     use Ada_Modbus.Transport.TLS_Mbed;
with Test_Certificates;

--  For semihosting output
with Semihosting;

procedure Main_TLS_Client is

   Connection : TLS_Connection;
   Config     : TLS_Config;
   Result     : Status;

   --  Buffers for Modbus communication
   PDU_Buf         : PDU_Buffer;
   PDU_Len         : Natural;
   ADU_Buf         : ADU_Buffer;
   ADU_Len         : Natural;
   Response_Buffer : Byte_Array (0 .. Max_ADU_Size - 1);
   Response_Length : Natural;

   --  Request parameters
   Transaction     : constant Transaction_Id := 1;
   Unit_Id_Val     : constant Unit_Id := 1;
   Start_Address   : constant Register_Address := 0;
   Quantity        : constant Register_Count := 10;

   --  Test server address (QEMU user-mode networking gateway)
   Server_Host : constant String := "10.0.2.2";
   Server_Port : constant := 8802;

   procedure Put_Line (S : String) renames Semihosting.Put_Line;

begin
   Put_Line ("=== Modbus TLS Client Test ===");
   Put_Line ("");

   --  Configure TLS with test certificates (certificate mode)
   Config := (
      Mode            => Auth_Certificate,
      CA_Certificate  => Test_Certificates.CA_Cert'Address,
      CA_Cert_Len     => Test_Certificates.CA_Cert'Length,
      Certificate     => Test_Certificates.Client_Cert'Address,
      Certificate_Len => Test_Certificates.Client_Cert'Length,
      Private_Key     => Test_Certificates.Client_Key'Address,
      Private_Key_Len => Test_Certificates.Client_Key'Length,
      Verify_Peer     => False,  --  Disable for testing with self-signed certs
      others          => <>
   );

   Put_Line ("Connecting to " & Server_Host & "...");

   --  Connect to TLS server
   Connect (Connection, Server_Host, Server_Port, Config, 10000, Result);

   if Result /= Success then
      Put_Line ("ERROR: Connection failed");
      Put_Line ("TLS Test FAILED");
      return;
   end if;

   Put_Line ("Connected successfully");

   --  Build Read Holding Registers request (FC 03)
   --  Step 1: Encode PDU
   Encode_Read_Registers_Request
     (Buffer        => PDU_Buf,
      Length        => PDU_Len,
      FC            => FC_Read_Holding_Registers,
      Start_Address => Start_Address,
      Quantity      => Quantity);

   --  Step 2: Build TCP frame (MBAP header + PDU)
   Build_Frame
     (ADU         => ADU_Buf,
      ADU_Length  => ADU_Len,
      Transaction => Transaction,
      Unit        => Unit_Id_Val,
      PDU         => PDU_Buf,
      PDU_Length  => PDU_Len);

   Put_Line ("Sending Read Holding Registers request...");

   --  Send request
   Send_Frame (Connection, ADU_Buf (0 .. ADU_Len - 1), Result);

   if Result /= Success then
      Put_Line ("ERROR: Send failed");
      Disconnect (Connection);
      Put_Line ("TLS Test FAILED");
      return;
   end if;

   Put_Line ("Request sent, waiting for response...");

   --  Receive response
   Receive_Frame (Connection, Response_Buffer, Response_Length, 5000, Result);

   if Result /= Success then
      Put_Line ("ERROR: Receive failed");
      Disconnect (Connection);
      Put_Line ("TLS Test FAILED");
      return;
   end if;

   Put_Line ("Received response");

   --  Verify response (basic check: length and function code)
   if Response_Length >= 9 then
      declare
         FC : constant Byte := Response_Buffer (7);  --  Function code after MBAP
      begin
         if FC = Byte (FC_Read_Holding_Registers) then
            Put_Line ("Response OK: FC 03");
            Put_Line ("");
            Put_Line ("TLS Test PASSED");
         elsif FC >= 16#80# then
            Put_Line ("ERROR: Exception response");
            Put_Line ("TLS Test FAILED");
         else
            Put_Line ("ERROR: Unexpected FC");
            Put_Line ("TLS Test FAILED");
         end if;
      end;
   else
      Put_Line ("ERROR: Response too short");
      Put_Line ("TLS Test FAILED");
   end if;

   --  Disconnect
   Disconnect (Connection);
   Put_Line ("Disconnected");

   --  Halt (light runtime has no exit)
   loop
      null;
   end loop;

end Main_TLS_Client;
