--  Main Loopback TLS Test - Server and Client in one binary
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  This test demonstrates TLS client-server communication within a single
--  QEMU instance using memory-based loopback (no real network).
--
--  Run in QEMU:
--    qemu-system-arm -M lm3s6965evb -nographic -semihosting \
--      -kernel bin/main_loopback_tls.elf

with Ada_Modbus;                        use Ada_Modbus;
with Ada_Modbus.Protocol;               use Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.TCP;           use Ada_Modbus.Protocol.TCP;
with Ada_Modbus.Transport.TLS_Mbed;     use Ada_Modbus.Transport.TLS_Mbed;
with Test_Certificates;
with Loopback;
with Semihosting;

procedure Main_Loopback_TLS is

   --  Server objects
   Server           : TLS_Server;
   Server_Conn      : TLS_Connection;
   Server_Config    : TLS_Config;

   --  Client objects
   Client_Conn      : TLS_Connection;
   Client_Config    : TLS_Config;

   Result           : Status;

   --  Buffers
   Request_PDU      : PDU_Buffer;
   Request_PDU_Len  : Natural;
   Request_ADU      : ADU_Buffer;
   Request_ADU_Len  : Natural;
   Response_Buf     : Byte_Array (0 .. Max_ADU_Size - 1);
   Response_Len     : Natural;

   --  Test parameters
   Transaction      : constant Transaction_Id := 42;
   Unit_Val         : constant Unit_Id := 1;
   Start_Addr       : constant Register_Address := 100;
   Quantity         : constant Register_Count := 5;

   --  Simulated holding registers (server data)
   Holding_Regs     : Register_Array (0 .. 199) := [others => 0];

   procedure Put_Line (S : String) renames Semihosting.Put_Line;

   --  Process Read Holding Registers request (server-side)
   procedure Handle_Request
     (Request  : Byte_Array;
      Req_Len  : Natural;
      Response : out Byte_Array;
      Resp_Len : out Natural)
   is
      Trans_Hi   : constant Byte := Request (0);
      Trans_Lo   : constant Byte := Request (1);
      Unit       : constant Byte := Request (6);
      FC         : constant Byte := Request (7);
      Addr       : Register_Address;
      Qty        : Natural;
      Byte_Count : Natural;
   begin
      pragma Unreferenced (Req_Len);

      if FC /= Byte (FC_Read_Holding_Registers) then
         --  Exception response: Illegal Function
         Response (0 .. 5) := Request (0 .. 5);
         Response (5) := 3;
         Response (6) := Unit;
         Response (7) := FC + 16#80#;
         Response (8) := 1;
         Resp_Len := 9;
         return;
      end if;

      Addr := Register_Address (Natural (Request (8)) * 256 +
                                Natural (Request (9)));
      Qty := Natural (Request (10)) * 256 + Natural (Request (11));

      if Qty < 1 or Qty > 125 or Natural (Addr) + Qty > Holding_Regs'Length then
         --  Exception response: Illegal Data Address
         Response (0 .. 5) := Request (0 .. 5);
         Response (5) := 3;
         Response (6) := Unit;
         Response (7) := FC + 16#80#;
         Response (8) := 2;
         Resp_Len := 9;
         return;
      end if;

      Byte_Count := Qty * 2;

      --  Build response
      Response (0) := Trans_Hi;
      Response (1) := Trans_Lo;
      Response (2) := 0;
      Response (3) := 0;
      Response (4) := Byte (((Byte_Count + 3) / 256) mod 256);
      Response (5) := Byte ((Byte_Count + 3) mod 256);
      Response (6) := Unit;
      Response (7) := FC;
      Response (8) := Byte (Byte_Count);

      for I in 0 .. Qty - 1 loop
         declare
            Val : constant Register_Value := Holding_Regs (Natural (Addr) + I);
            Idx : constant Natural := 9 + I * 2;
         begin
            Response (Idx)     := Byte (Val / 256);
            Response (Idx + 1) := Byte (Val mod 256);
         end;
      end loop;

      Resp_Len := 9 + Byte_Count;
   end Handle_Request;

begin
   Put_Line ("=== Modbus TLS Loopback Test ===");
   Put_Line ("");

   --  Initialize test data
   for I in Holding_Regs'Range loop
      Holding_Regs (I) := Register_Value (I * 10);
   end loop;

   --  Reset loopback buffers
   Loopback.Reset;
   Put_Line ("Loopback buffers reset");

   --  Configure TLS (shared certs for simplicity)
   Server_Config := (
      Mode            => Auth_Certificate,
      Certificate     => Test_Certificates.Server_Cert'Address,
      Certificate_Len => Test_Certificates.Server_Cert'Length,
      Private_Key     => Test_Certificates.Server_Key'Address,
      Private_Key_Len => Test_Certificates.Server_Key'Length,
      CA_Certificate  => Test_Certificates.CA_Cert'Address,
      CA_Cert_Len     => Test_Certificates.CA_Cert'Length,
      Verify_Peer     => False,
      others          => <>
   );

   Client_Config := (
      Mode            => Auth_Certificate,
      CA_Certificate  => Test_Certificates.CA_Cert'Address,
      CA_Cert_Len     => Test_Certificates.CA_Cert'Length,
      Certificate     => Test_Certificates.Client_Cert'Address,
      Certificate_Len => Test_Certificates.Client_Cert'Length,
      Private_Key     => Test_Certificates.Client_Key'Address,
      Private_Key_Len => Test_Certificates.Client_Key'Length,
      Verify_Peer     => False,
      others          => <>
   );

   ---------------------
   --  1. Server Listen
   ---------------------
   Put_Line ("1. Server: Starting listener...");
   Loopback.Set_Endpoint (Loopback.Server_Endpoint);
   Listen (Server, 802, Server_Config, Result);
   if Result /= Success then
      Put_Line ("ERROR: Server listen failed");
      Put_Line ("LOOPBACK TEST FAILED");
      loop null; end loop;
   end if;
   Put_Line ("   Server listening on port 802");

   ----------------------
   --  2. Client Connect
   ----------------------
   Put_Line ("2. Client: Connecting...");
   Loopback.Set_Endpoint (Loopback.Client_Endpoint);
   Connect (Client_Conn, "127.0.0.1", 802, Client_Config, 5000, Result);
   if Result /= Success then
      Put_Line ("ERROR: Client connect failed");
      Put_Line ("LOOPBACK TEST FAILED");
      loop null; end loop;
   end if;
   Put_Line ("   Client connected");

   --------------------------
   --  3. Client Send Request
   --------------------------
   Put_Line ("3. Client: Sending Read Holding Registers request...");

   --  Build PDU
   Encode_Read_Registers_Request
     (Buffer        => Request_PDU,
      Length        => Request_PDU_Len,
      FC            => FC_Read_Holding_Registers,
      Start_Address => Start_Addr,
      Quantity      => Quantity);

   --  Build ADU (MBAP + PDU)
   Build_Frame
     (ADU         => Request_ADU,
      ADU_Length  => Request_ADU_Len,
      Transaction => Transaction,
      Unit        => Unit_Val,
      PDU         => Request_PDU,
      PDU_Length  => Request_PDU_Len);

   Loopback.Set_Endpoint (Loopback.Client_Endpoint);
   Send_Frame (Client_Conn, Request_ADU (0 .. Request_ADU_Len - 1), Result);
   if Result /= Success then
      Put_Line ("ERROR: Client send failed");
      Put_Line ("LOOPBACK TEST FAILED");
      loop null; end loop;
   end if;
   Put_Line ("   Request sent (12 bytes)");

   ---------------------------
   --  4. Server Accept Client
   ---------------------------
   Put_Line ("4. Server: Accepting connection...");
   Loopback.Set_Endpoint (Loopback.Server_Endpoint);
   Accept_Connection (Server, Server_Conn, 1000, Result);
   if Result /= Success then
      Put_Line ("ERROR: Server accept failed");
      Put_Line ("LOOPBACK TEST FAILED");
      loop null; end loop;
   end if;
   Put_Line ("   Client accepted");

   -----------------------------
   --  5. Server Receive Request
   -----------------------------
   Put_Line ("5. Server: Receiving request...");
   Loopback.Set_Endpoint (Loopback.Server_Endpoint);
   Receive_Frame (Server_Conn, Response_Buf, Response_Len, 1000, Result);
   if Result /= Success then
      Put_Line ("ERROR: Server receive failed");
      Put_Line ("LOOPBACK TEST FAILED");
      loop null; end loop;
   end if;
   Put_Line ("   Request received");

   -----------------------------------
   --  6. Server Process & Send Response
   -----------------------------------
   Put_Line ("6. Server: Processing and sending response...");
   declare
      Server_Response : Byte_Array (0 .. Max_ADU_Size - 1);
      Server_Resp_Len : Natural;
   begin
      Handle_Request (Response_Buf, Response_Len, Server_Response, Server_Resp_Len);

      Loopback.Set_Endpoint (Loopback.Server_Endpoint);
      Send_Frame (Server_Conn, Server_Response (0 .. Server_Resp_Len - 1), Result);
      if Result /= Success then
         Put_Line ("ERROR: Server send failed");
         Put_Line ("LOOPBACK TEST FAILED");
         loop null; end loop;
      end if;
      Put_Line ("   Response sent");
   end;

   --------------------------------
   --  7. Client Receive Response
   --------------------------------
   Put_Line ("7. Client: Receiving response...");
   Loopback.Set_Endpoint (Loopback.Client_Endpoint);
   Receive_Frame (Client_Conn, Response_Buf, Response_Len, 1000, Result);
   if Result /= Success then
      Put_Line ("ERROR: Client receive failed");
      Put_Line ("LOOPBACK TEST FAILED");
      loop null; end loop;
   end if;
   Put_Line ("   Response received");

   ---------------------------
   --  8. Validate Response
   ---------------------------
   Put_Line ("8. Validating response...");
   declare
      Expected_FC : constant Byte := Byte (FC_Read_Holding_Registers);
      Received_FC : Byte;
      Valid       : Boolean := True;
   begin
      if Response_Len < 9 then
         Put_Line ("   ERROR: Response too short");
         Valid := False;
      else
         Received_FC := Response_Buf (7);
         if Received_FC /= Expected_FC then
            Put_Line ("   ERROR: Wrong function code");
            Valid := False;
         else
            --  Check first register value
            --  Address 100, value should be 100*10 = 1000 = 0x03E8
            declare
               Hi : constant Byte := Response_Buf (9);
               Lo : constant Byte := Response_Buf (10);
               Val : constant Natural := Natural (Hi) * 256 + Natural (Lo);
            begin
               if Val /= 1000 then
                  Put_Line ("   ERROR: Wrong register value");
                  Valid := False;
               end if;
            end;
         end if;
      end if;

      if Valid then
         Put_Line ("   Response valid!");
         Put_Line ("");
         Put_Line ("LOOPBACK TEST PASSED");
      else
         Put_Line ("LOOPBACK TEST FAILED");
      end if;
   end;

   --  Cleanup
   Loopback.Set_Endpoint (Loopback.Client_Endpoint);
   Disconnect (Client_Conn);
   Loopback.Set_Endpoint (Loopback.Server_Endpoint);
   Disconnect (Server_Conn);
   Close_Server (Server);

   Put_Line ("Test complete.");

   --  Halt
   loop
      null;
   end loop;

end Main_Loopback_TLS;
