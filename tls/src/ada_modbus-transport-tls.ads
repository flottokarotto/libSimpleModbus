--  Ada_Modbus.Transport.TLS - TLS/SSL Transport Layer
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides secure Modbus/TCP communication using TLS.
--  Uses AWS (Ada Web Server) SSL implementation.
--  Default port: 802 (Modbus/TCP Security)

with Ada.Strings.Unbounded;
with AWS.Net.SSL;

package Ada_Modbus.Transport.TLS is

   use Ada.Strings.Unbounded;

   type TLS_Connection is tagged limited private;

   type TLS_Config is record
      Certificate_File : Unbounded_String := Null_Unbounded_String;
      Private_Key_File : Unbounded_String := Null_Unbounded_String;
      CA_File          : Unbounded_String := Null_Unbounded_String;
      Verify_Peer      : Boolean := True;
   end record;

   Default_TLS_Port : constant := 802;  --  Modbus/TCP Security port

   --  Connection state
   type Connection_State is (Disconnected, Connected, Error);

   function State (Connection : TLS_Connection) return Connection_State;

   function Last_Error (Connection : TLS_Connection) return String;

   -----------------------
   --  Client Functions --
   -----------------------

   procedure Connect
     (Connection : in out TLS_Connection;
      Host       : String;
      Port       : Natural := Default_TLS_Port;
      Config     : TLS_Config;
      Timeout_Ms : Natural := 5000;
      Result     : out Status);
   --  Connect to a Modbus/TCP Security server

   procedure Disconnect (Connection : in out TLS_Connection);
   --  Disconnect from server

   -----------------------
   --  Server Functions --
   -----------------------

   procedure Listen
     (Connection : in out TLS_Connection;
      Port       : Natural := Default_TLS_Port;
      Config     : TLS_Config;
      Result     : out Status);
   --  Start listening for TLS connections

   procedure Accept_Connection
     (Server     : TLS_Connection;
      Client     : out TLS_Connection;
      Timeout_Ms : Natural := 0;
      Result     : out Status);
   --  Accept incoming TLS connection (0 = wait forever)

   procedure Close_Server (Connection : in out TLS_Connection);
   --  Stop listening

   -----------------
   --  Frame I/O  --
   -----------------

   procedure Send_Frame
     (Connection : in out TLS_Connection;
      Frame      : Byte_Array;
      Result     : out Status);
   --  Send a complete Modbus frame

   procedure Receive_Frame
     (Connection : in out TLS_Connection;
      Frame      : out Byte_Array;
      Length     : out Natural;
      Timeout_Ms : Natural := 1000;
      Result     : out Status);
   --  Receive a complete Modbus frame

private

   type TLS_Connection is tagged limited record
      Socket       : AWS.Net.SSL.Socket_Type;
      SSL_Config   : AWS.Net.SSL.Config;
      Server_Mode  : Boolean := False;
      Current_State : Connection_State := Disconnected;
      Error_Message : Unbounded_String := Null_Unbounded_String;
   end record;

end Ada_Modbus.Transport.TLS;
