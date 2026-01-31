--  TLS Integration Test Runner
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Tests the TLS transport layer against a Python TLS Modbus server.
--
--  Prerequisites:
--    1. Generate test certificates in tests/integration/certs/
--    2. Start the TLS server:
--       python tests/integration/tls_modbus_server.py \
--         --cert tests/integration/certs/server.crt \
--         --key tests/integration/certs/server.key
--
--  Run:
--    ./bin/tls_integration_test_runner

with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Command_Line;

with Ada_Modbus;                    use Ada_Modbus;
with Ada_Modbus.Protocol;           use Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.TCP;       use Ada_Modbus.Protocol.TCP;
with Ada_Modbus.Transport.TLS;      use Ada_Modbus.Transport.TLS;

procedure TLS_Integration_Test_Runner is

   --  Configuration
   Server_Host : constant String := "127.0.0.1";
   Server_Port : constant := 8802;
   Cert_Dir    : constant String := "tests/integration/certs/";

   --  Test counters
   Tests_Run    : Natural := 0;
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;

   procedure Report (Name : String; Passed : Boolean) is
   begin
      Tests_Run := Tests_Run + 1;
      if Passed then
         Tests_Passed := Tests_Passed + 1;
         Put_Line ("  [PASS] " & Name);
      else
         Tests_Failed := Tests_Failed + 1;
         Put_Line ("  [FAIL] " & Name);
      end if;
   end Report;

   procedure Test_Connect_Disconnect is
      Connection : TLS_Connection;
      Config     : TLS_Config;
      Result     : Status;
   begin
      Put_Line ("Test: Connect and Disconnect");

      Config := (
         Certificate_File => Null_Unbounded_String,
         Private_Key_File => Null_Unbounded_String,
         CA_File          => To_Unbounded_String (Cert_Dir & "ca.crt"),
         Verify_Peer      => True
      );

      Connect (Connection, Server_Host, Server_Port, Config, 5000, Result);
      Report ("Connect to TLS server", Result = Success);

      if Result = Success then
         Report ("Connection state is Connected", State (Connection) = Connected);
         Disconnect (Connection);
         Report ("Disconnect", State (Connection) = Disconnected);
      end if;
   end Test_Connect_Disconnect;

   procedure Test_Read_Holding_Registers is
      Connection : TLS_Connection;
      Config     : TLS_Config;
      Result     : Status;

      --  Buffers
      PDU_Buf     : PDU_Buffer;
      PDU_Len     : Natural;
      ADU_Buf     : ADU_Buffer;
      ADU_Len     : Natural;
      Resp_Buf    : Byte_Array (0 .. Max_ADU_Size - 1);
      Resp_Len    : Natural;

      --  Request parameters
      Transaction : constant Transaction_Id := 1;
      Unit_Val    : constant Unit_Id := 1;
      Start_Addr  : constant Register_Address := 0;
      Quantity    : constant Register_Count := 10;

      --  Expected values (from modbus_simulator.py)
      Expected    : constant array (0 .. 9) of Register_Value :=
        [0, 100, 200, 300, 400, 500, 600, 700, 800, 900];
   begin
      Put_Line ("Test: Read Holding Registers over TLS");

      Config := (
         Certificate_File => Null_Unbounded_String,
         Private_Key_File => Null_Unbounded_String,
         CA_File          => To_Unbounded_String (Cert_Dir & "ca.crt"),
         Verify_Peer      => True
      );

      Connect (Connection, Server_Host, Server_Port, Config, 5000, Result);
      if Result /= Success then
         Report ("Connect for read test", False);
         return;
      end if;

      --  Build request
      Encode_Read_Registers_Request
        (Buffer        => PDU_Buf,
         Length        => PDU_Len,
         FC            => FC_Read_Holding_Registers,
         Start_Address => Start_Addr,
         Quantity      => Quantity);

      Build_Frame
        (ADU         => ADU_Buf,
         ADU_Length  => ADU_Len,
         Transaction => Transaction,
         Unit        => Unit_Val,
         PDU         => PDU_Buf,
         PDU_Length  => PDU_Len);

      --  Send request
      Send_Frame (Connection, ADU_Buf (0 .. ADU_Len - 1), Result);
      Report ("Send Read Holding Registers request", Result = Success);

      if Result /= Success then
         Disconnect (Connection);
         return;
      end if;

      --  Receive response
      Receive_Frame (Connection, Resp_Buf, Resp_Len, 5000, Result);
      Report ("Receive response", Result = Success);

      if Result = Success then
         --  Verify response
         declare
            FC : constant Byte := Resp_Buf (7);
            Valid : Boolean := True;
         begin
            if FC /= Byte (FC_Read_Holding_Registers) then
               Report ("Response function code", False);
               Valid := False;
            else
               Report ("Response function code", True);
            end if;

            --  Check register values
            if Valid and Resp_Len >= 9 + 20 then
               for I in 0 .. 9 loop
                  declare
                     Idx : constant Natural := 9 + I * 2;
                     Val : constant Register_Value :=
                       Register_Value (Resp_Buf (Idx)) * 256 +
                       Register_Value (Resp_Buf (Idx + 1));
                  begin
                     if Val /= Expected (I) then
                        Put_Line ("    Register " & I'Image &
                                  ": expected" & Expected (I)'Image &
                                  ", got" & Val'Image);
                        Valid := False;
                     end if;
                  end;
               end loop;
               Report ("Register values match expected", Valid);
            end if;
         end;
      end if;

      Disconnect (Connection);
   end Test_Read_Holding_Registers;

   procedure Test_Write_Single_Register is
      Connection : TLS_Connection;
      Config     : TLS_Config;
      Result     : Status;

      PDU_Buf     : PDU_Buffer;
      PDU_Len     : Natural;
      ADU_Buf     : ADU_Buffer;
      ADU_Len     : Natural;
      Resp_Buf    : Byte_Array (0 .. Max_ADU_Size - 1);
      Resp_Len    : Natural;

      Transaction : constant Transaction_Id := 2;
      Unit_Val    : constant Unit_Id := 1;
      Reg_Addr    : constant Register_Address := 50;
      Reg_Value   : constant Register_Value := 16#ABCD#;
   begin
      Put_Line ("Test: Write Single Register over TLS");

      Config := (
         Certificate_File => Null_Unbounded_String,
         Private_Key_File => Null_Unbounded_String,
         CA_File          => To_Unbounded_String (Cert_Dir & "ca.crt"),
         Verify_Peer      => True
      );

      Connect (Connection, Server_Host, Server_Port, Config, 5000, Result);
      if Result /= Success then
         Report ("Connect for write test", False);
         return;
      end if;

      --  Build write request
      Encode_Write_Single_Register_Request
        (Buffer  => PDU_Buf,
         Length  => PDU_Len,
         Address => Reg_Addr,
         Value   => Reg_Value);

      Build_Frame
        (ADU         => ADU_Buf,
         ADU_Length  => ADU_Len,
         Transaction => Transaction,
         Unit        => Unit_Val,
         PDU         => PDU_Buf,
         PDU_Length  => PDU_Len);

      Send_Frame (Connection, ADU_Buf (0 .. ADU_Len - 1), Result);
      Report ("Send Write Single Register request", Result = Success);

      if Result /= Success then
         Disconnect (Connection);
         return;
      end if;

      Receive_Frame (Connection, Resp_Buf, Resp_Len, 5000, Result);
      Report ("Receive write response", Result = Success);

      if Result = Success then
         --  Write response should echo the request
         declare
            FC : constant Byte := Resp_Buf (7);
         begin
            Report ("Write response function code",
                    FC = Byte (FC_Write_Single_Register));
         end;
      end if;

      Disconnect (Connection);
   end Test_Write_Single_Register;

   procedure Test_Connection_Without_Verification is
      Connection : TLS_Connection;
      Config     : TLS_Config;
      Result     : Status;
   begin
      Put_Line ("Test: Connect without peer verification");

      Config := (
         Certificate_File => Null_Unbounded_String,
         Private_Key_File => Null_Unbounded_String,
         CA_File          => Null_Unbounded_String,
         Verify_Peer      => False
      );

      Connect (Connection, Server_Host, Server_Port, Config, 5000, Result);
      Report ("Connect without verification", Result = Success);

      if Result = Success then
         Disconnect (Connection);
      end if;
   end Test_Connection_Without_Verification;

begin
   Put_Line ("===========================================");
   Put_Line ("  AdaModbus TLS Integration Tests");
   Put_Line ("===========================================");
   Put_Line ("");
   Put_Line ("Server: " & Server_Host & ":" & Server_Port'Image);
   Put_Line ("Certificates: " & Cert_Dir);
   Put_Line ("");

   --  Run tests
   Test_Connect_Disconnect;
   Put_Line ("");

   Test_Read_Holding_Registers;
   Put_Line ("");

   Test_Write_Single_Register;
   Put_Line ("");

   Test_Connection_Without_Verification;
   Put_Line ("");

   --  Summary
   Put_Line ("===========================================");
   Put_Line ("  Test Summary");
   Put_Line ("===========================================");
   Put_Line ("  Total:  " & Tests_Run'Image);
   Put_Line ("  Passed: " & Tests_Passed'Image);
   Put_Line ("  Failed: " & Tests_Failed'Image);
   Put_Line ("");

   if Tests_Failed = 0 then
      Put_Line ("TLS INTEGRATION TESTS PASSED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   else
      Put_Line ("TLS INTEGRATION TESTS FAILED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end TLS_Integration_Test_Runner;
