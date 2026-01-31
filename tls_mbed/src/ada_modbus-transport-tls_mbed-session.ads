--  Ada_Modbus.Transport.TLS_Mbed.Session - TLS Session Resumption
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides TLS session saving and restoration for faster reconnects.
--  This is important for embedded systems with frequent connection drops.
--
--  Session resumption significantly reduces handshake time and CPU usage
--  by reusing cryptographic keys from a previous connection.
--
--  Usage (Client-side):
--    1. After successful Connect, call Save_Session to store session data
--    2. On reconnect, call Restore_Session before Connect
--    3. The handshake will use abbreviated form if server accepts
--
--  Example:
--    --  First connection
--    Connect (Conn, Host, Port, Config, Result);
--    if Result = Success then
--       Save_Session (Conn, Saved_Session, Result);
--    end if;
--
--    --  Later reconnection (after disconnect)
--    Restore_Session (Conn, Saved_Session, Result);
--    Connect (Conn, Host, Port, Config, Result);  --  Uses saved session
--
--  Notes:
--    - Sessions have a limited lifetime (typically minutes to hours)
--    - The server decides whether to accept session resumption
--    - PSK mode also supports session resumption

package Ada_Modbus.Transport.TLS_Mbed.Session is

   pragma Preelaborate;

   --  Saved TLS session data
   --  This is an opaque type that holds session resumption data
   type Saved_Session is limited private;

   --  Maximum session data size (should be enough for TLS 1.2/1.3)
   Max_Session_Size : constant := 256;

   --  Save session from an active connection
   --  Call after successful Connect and handshake
   --  The session can be used for later reconnection
   --
   --  Connection: Active TLS connection
   --  Session: Will hold the saved session data
   --  Result: Success if session was saved successfully
   procedure Save_Session
     (Connection : TLS_Connection;
      Session    : out Saved_Session;
      Result     : out Status)
     with Post => (if Result = Success then Is_Valid (Session));

   --  Restore session for reconnection
   --  Call on a new/reset connection BEFORE Connect
   --  The next Connect will attempt session resumption
   --
   --  Connection: New or reset TLS connection (not connected)
   --  Session: Previously saved session
   --  Result: Success if session was restored to connection
   procedure Restore_Session
     (Connection : in out TLS_Connection;
      Session    : Saved_Session;
      Result     : out Status)
     with Pre => Is_Valid (Session);

   --  Check if a saved session is valid
   --  Returns True if session data has been saved
   function Is_Valid (Session : Saved_Session) return Boolean;

   --  Clear saved session data
   procedure Clear (Session : in out Saved_Session)
     with Post => not Is_Valid (Session);

private

   --  Session storage - matches MbedTLS.SSL.SSL_Session size
   type Session_Buffer is array (1 .. Max_Session_Size) of Byte
     with Convention => C;

   type Saved_Session is limited record
      Data      : Session_Buffer := [others => 0];
      Is_Saved  : Boolean := False;
   end record;

end Ada_Modbus.Transport.TLS_Mbed.Session;
