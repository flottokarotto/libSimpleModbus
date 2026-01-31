--  Ada_Modbus.Transport.TLS - TLS/SSL Transport Layer Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada.Exceptions;
with Ada.Streams;
with AWS.Net;
with AWS.Net.Std;

package body Ada_Modbus.Transport.TLS is

   use Ada.Streams;

   procedure Set_Error
     (Connection : in out TLS_Connection;
      Message    : String);

   procedure Set_Error
     (Connection : in out TLS_Connection;
      Message    : String)
   is
   begin
      Connection.Current_State := Error;
      Connection.Error_Message := To_Unbounded_String (Message);
   end Set_Error;

   -----------
   -- State --
   -----------

   function State (Connection : TLS_Connection) return Connection_State is
   begin
      return Connection.Current_State;
   end State;

   ----------------
   -- Last_Error --
   ----------------

   function Last_Error (Connection : TLS_Connection) return String is
   begin
      return To_String (Connection.Error_Message);
   end Last_Error;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Connection : in out TLS_Connection;
      Host       : String;
      Port       : Natural := Default_TLS_Port;
      Config     : TLS_Config;
      Timeout_Ms : Natural := 5000;
      Result     : out Status)
   is
      use AWS.Net.SSL;
   begin
      Connection.Current_State := Disconnected;
      Connection.Error_Message := Null_Unbounded_String;
      Connection.Server_Mode := False;

      --  Initialize SSL config
      Initialize
        (Config               => Connection.SSL_Config,
         Security_Mode        => TLS_Client,
         Client_Certificate   => To_String (Config.Certificate_File),
         Server_Key           => To_String (Config.Private_Key_File),
         Trusted_CA_Filename  => To_String (Config.CA_File),
         Check_Certificate    => Config.Verify_Peer,
         Exchange_Certificate => Config.Certificate_File /= Null_Unbounded_String);

      --  Set config on socket
      AWS.Net.SSL.Set_Config (Connection.Socket, Connection.SSL_Config);

      --  Set timeout using dispatching
      Connection.Socket.Set_Timeout (Duration (Timeout_Ms) / 1000.0);

      --  Connect
      AWS.Net.SSL.Connect
        (Socket => Connection.Socket,
         Host   => Host,
         Port   => Port,
         Wait   => True);

      Connection.Current_State := Connected;
      Result := Success;

   exception
      when E : AWS.Net.Socket_Error =>
         Set_Error (Connection, "TLS connect: " & Ada.Exceptions.Exception_Message (E));
         Result := Frame_Error;
      when E : others =>
         Set_Error (Connection, "TLS connect: " & Ada.Exceptions.Exception_Message (E));
         Result := Frame_Error;
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Connection : in out TLS_Connection) is
   begin
      if Connection.Current_State = Connected then
         AWS.Net.SSL.Shutdown (Connection.Socket);
      end if;
      Connection.Current_State := Disconnected;
   exception
      when others =>
         Connection.Current_State := Disconnected;
   end Disconnect;

   ------------
   -- Listen --
   ------------

   procedure Listen
     (Connection : in out TLS_Connection;
      Port       : Natural := Default_TLS_Port;
      Config     : TLS_Config;
      Result     : out Status)
   is
      use AWS.Net.SSL;
   begin
      Connection.Current_State := Disconnected;
      Connection.Error_Message := Null_Unbounded_String;
      Connection.Server_Mode := True;

      --  Initialize SSL config for server
      Initialize
        (Config               => Connection.SSL_Config,
         Security_Mode        => TLS_Server,
         Server_Certificate   => To_String (Config.Certificate_File),
         Server_Key           => To_String (Config.Private_Key_File),
         Trusted_CA_Filename  => To_String (Config.CA_File),
         Check_Certificate    => Config.Verify_Peer,
         Exchange_Certificate => Config.Verify_Peer);

      --  Set config on socket
      AWS.Net.SSL.Set_Config (Connection.Socket, Connection.SSL_Config);

      --  Bind and listen
      AWS.Net.Std.Bind
        (AWS.Net.Std.Socket_Type (Connection.Socket),
         Port => Port);
      AWS.Net.Std.Listen
        (AWS.Net.Std.Socket_Type (Connection.Socket));

      Connection.Current_State := Connected;
      Result := Success;

   exception
      when AWS.Net.Socket_Error =>
         Set_Error (Connection, "TLS listen failed");
         Result := Frame_Error;
      when others =>
         Set_Error (Connection, "TLS listen failed (unknown error)");
         Result := Frame_Error;
   end Listen;

   -----------------------
   -- Accept_Connection --
   -----------------------

   procedure Accept_Connection
     (Server     : TLS_Connection;
      Client     : out TLS_Connection;
      Timeout_Ms : Natural := 0;
      Result     : out Status)
   is
      pragma Unreferenced (Timeout_Ms);  --  TODO: implement timeout
   begin
      Client.Server_Mode := False;
      Client.Current_State := Disconnected;
      Client.Error_Message := Null_Unbounded_String;

      --  Copy SSL config from server
      AWS.Net.SSL.Set_Config (Client.Socket, Server.SSL_Config);

      --  Accept connection
      AWS.Net.SSL.Accept_Socket (Server.Socket, Client.Socket);

      Client.Current_State := Connected;
      Result := Success;

   exception
      when AWS.Net.Socket_Error =>
         Client.Current_State := Error;
         Client.Error_Message := To_Unbounded_String ("TLS accept failed");
         Result := Frame_Error;
      when others =>
         Client.Current_State := Error;
         Client.Error_Message := To_Unbounded_String ("TLS accept failed");
         Result := Frame_Error;
   end Accept_Connection;

   ------------------
   -- Close_Server --
   ------------------

   procedure Close_Server (Connection : in out TLS_Connection) is
   begin
      AWS.Net.SSL.Shutdown (Connection.Socket);
      Connection.Current_State := Disconnected;
   exception
      when others =>
         Connection.Current_State := Disconnected;
   end Close_Server;

   ----------------
   -- Send_Frame --
   ----------------

   procedure Send_Frame
     (Connection : in out TLS_Connection;
      Frame      : Byte_Array;
      Result     : out Status)
   is
      Data : Stream_Element_Array (1 .. Frame'Length);
      Last : Stream_Element_Offset;
   begin
      if Connection.Current_State /= Connected then
         Result := Frame_Error;
         return;
      end if;

      --  Convert Byte_Array to Stream_Element_Array
      for I in Frame'Range loop
         Data (Stream_Element_Offset (I - Frame'First + 1)) :=
           Stream_Element (Frame (I));
      end loop;

      AWS.Net.SSL.Send (Connection.Socket, Data, Last);

      if Last = Data'Last then
         Result := Success;
      else
         Set_Error (Connection, "TLS send incomplete");
         Result := Frame_Error;
      end if;

   exception
      when AWS.Net.Socket_Error =>
         Set_Error (Connection, "TLS send failed");
         Result := Frame_Error;
      when others =>
         Set_Error (Connection, "TLS send failed (unknown error)");
         Result := Frame_Error;
   end Send_Frame;

   -------------------
   -- Receive_Frame --
   -------------------

   procedure Receive_Frame
     (Connection : in out TLS_Connection;
      Frame      : out Byte_Array;
      Length     : out Natural;
      Timeout_Ms : Natural := 1000;
      Result     : out Status)
   is
      Header     : Stream_Element_Array (1 .. 7);  --  MBAP header
      Header_Last : Stream_Element_Offset;
      Payload_Len : Natural;
      Payload    : Stream_Element_Array (1 .. 253);  --  Max PDU
      Payload_Last : Stream_Element_Offset;
   begin
      Length := 0;
      Frame := [others => 0];

      if Connection.Current_State /= Connected then
         Result := Frame_Error;
         return;
      end if;

      --  Set receive timeout using dispatching call
      Connection.Socket.Set_Timeout (Duration (Timeout_Ms) / 1000.0);

      --  Read MBAP header (7 bytes)
      AWS.Net.SSL.Receive (Connection.Socket, Header, Header_Last);

      if Header_Last < 7 then
         Result := Timeout;
         return;
      end if;

      --  Extract length from MBAP header (bytes 5-6, big-endian)
      --  Length field includes Unit ID (1 byte) + PDU
      Payload_Len := Natural (Header (5)) * 256 + Natural (Header (6)) - 1;

      if Payload_Len > 253 or Payload_Len > Frame'Length - 7 then
         Set_Error (Connection, "Frame too large");
         Result := Frame_Error;
         return;
      end if;

      --  Read remaining payload (Unit ID already in header byte 7)
      if Payload_Len > 0 then
         AWS.Net.SSL.Receive
           (Connection.Socket,
            Payload (1 .. Stream_Element_Offset (Payload_Len)),
            Payload_Last);

         if Natural (Payload_Last) < Payload_Len then
            Result := Timeout;
            return;
         end if;
      end if;

      --  Copy header to output
      for I in 1 .. 7 loop
         Frame (Frame'First + I - 1) := Byte (Header (Stream_Element_Offset (I)));
      end loop;

      --  Copy payload to output
      for I in 1 .. Payload_Len loop
         Frame (Frame'First + 6 + I) := Byte (Payload (Stream_Element_Offset (I)));
      end loop;

      Length := 7 + Payload_Len;
      Result := Success;

   exception
      when AWS.Net.Socket_Error =>
         Result := Timeout;
      when others =>
         Set_Error (Connection, "TLS receive failed");
         Result := Frame_Error;
   end Receive_Frame;

end Ada_Modbus.Transport.TLS;
