--  Ada_Modbus.Transport.TLS_Mbed.Session - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada.Unchecked_Conversion;
with System;
with MbedTLS.SSL;

package body Ada_Modbus.Transport.TLS_Mbed.Session is

   use type MbedTLS.Error_Code;

   --  Get SSL context from connection (internal use)
   --  Note: This requires access to parent package internals
   --  Using overlay to access the SSL context storage

   type SSL_Context_Access is access all MbedTLS.SSL.SSL_Context;

   function Get_SSL_Ctx (Connection : TLS_Connection)
      return SSL_Context_Access
   is
      --  The SSL context is at the start of TLS_Connection
      function To_SSL_Ctx is new Ada.Unchecked_Conversion
        (System.Address, SSL_Context_Access);
   begin
      return To_SSL_Ctx (Connection.SSL_Ctx'Address);
   end Get_SSL_Ctx;

   ------------------
   -- Save_Session --
   ------------------

   procedure Save_Session
     (Connection : TLS_Connection;
      Session    : out Saved_Session;
      Result     : out Status)
   is
      Temp_Session : aliased MbedTLS.SSL.SSL_Session;
      Ret : MbedTLS.Error_Code;
   begin
      --  Initialize output
      Session.Is_Saved := False;
      Session.Data := [others => 0];

      --  Check connection state
      if Connection.Current_State /= Connected then
         Result := Frame_Error;
         return;
      end if;

      --  Initialize temporary session
      MbedTLS.SSL.Session_Init (Temp_Session);

      --  Get session from connection
      Ret := MbedTLS.SSL.Get_Session
        (Get_SSL_Ctx (Connection).all, Temp_Session);

      if Ret /= MbedTLS.Success then
         MbedTLS.SSL.Session_Free (Temp_Session);
         Result := Frame_Error;
         return;
      end if;

      --  Copy session data to our storage
      --  Note: This is a byte-for-byte copy of the mbedTLS session structure
      declare
         Src : Session_Buffer
           with Import, Address => Temp_Session'Address;
      begin
         Session.Data := Src;
      end;

      Session.Is_Saved := True;
      MbedTLS.SSL.Session_Free (Temp_Session);
      Result := Success;
   end Save_Session;

   ---------------------
   -- Restore_Session --
   ---------------------

   procedure Restore_Session
     (Connection : in out TLS_Connection;
      Session    : Saved_Session;
      Result     : out Status)
   is
   begin
      --  Check if session is valid
      if not Session.Is_Saved then
         Result := Invalid_Request;
         return;
      end if;

      --  Store session data in connection for use during Connect
      --  The session will be applied after SSL Setup but before Handshake
      declare
         Dst : Session_Buffer
           with Import, Address => Connection.Pending_Session'Address;
      begin
         Dst := Session.Data;
      end;

      Connection.Has_Pending_Session := True;
      Result := Success;
   end Restore_Session;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Session : Saved_Session) return Boolean is
   begin
      return Session.Is_Saved;
   end Is_Valid;

   -----------
   -- Clear --
   -----------

   procedure Clear (Session : in out Saved_Session) is
   begin
      Session.Data := [others => 0];
      Session.Is_Saved := False;
   end Clear;

end Ada_Modbus.Transport.TLS_Mbed.Session;
