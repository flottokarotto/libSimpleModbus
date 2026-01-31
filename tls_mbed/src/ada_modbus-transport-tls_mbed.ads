--  Ada_Modbus.Transport.TLS_Mbed - TLS/SSL Transport Layer using mbedTLS
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides secure Modbus/TCP communication using mbedTLS.
--  Designed for embedded MCU targets (ZFP/Light runtime compatible).
--  Default port: 802 (Modbus/TCP Security)
--
--  Usage:
--    1. Prepare certificates in DER format (not PEM)
--    2. Store certificates in Flash or static arrays
--    3. Configure TLS_Config with certificate addresses and lengths
--    4. Call Connect, Send_Frame, Receive_Frame, Disconnect

with System;

package Ada_Modbus.Transport.TLS_Mbed is

   pragma Preelaborate;

   type TLS_Connection is limited private;

   --  Authentication mode
   type Auth_Mode is (Auth_Certificate, Auth_PSK);

   --  TLS configuration for embedded systems
   --  Certificates should be in DER format (binary, not PEM)
   type TLS_Config is record
      --  Authentication mode (Certificate or PSK)
      Mode            : Auth_Mode := Auth_Certificate;

      --  Certificate-based authentication (Mode = Auth_Certificate)
      --  Client certificate (optional for client, required for mutual auth)
      Certificate     : System.Address := System.Null_Address;
      Certificate_Len : Natural := 0;

      --  Client private key
      Private_Key     : System.Address := System.Null_Address;
      Private_Key_Len : Natural := 0;

      --  CA certificate for server verification
      CA_Certificate  : System.Address := System.Null_Address;
      CA_Cert_Len     : Natural := 0;

      --  Pre-Shared Key authentication (Mode = Auth_PSK)
      --  PSK: typically 16-32 bytes shared secret
      PSK             : System.Address := System.Null_Address;
      PSK_Len         : Natural := 0;

      --  PSK Identity: null-terminated string identifying this client/server
      PSK_Identity    : System.Address := System.Null_Address;
      PSK_Identity_Len : Natural := 0;

      --  Enable peer certificate verification (only for Auth_Certificate mode)
      Verify_Peer     : Boolean := True;
   end record;

   Default_TLS_Port : constant := 802;  --  Modbus/TCP Security port

   --  Maximum hostname length for Connect
   Max_Hostname_Length : constant := 64;

   --  Connection state
   type Connection_State is (Disconnected, Connected, Handshake_Failed, Error);

   --  Detailed TLS error categories for debugging
   type TLS_Error_Category is
     (No_Error,
      --  Connection errors
      Socket_Error,           --  Failed to create/bind socket
      Connect_Failed,         --  TCP connection failed
      DNS_Error,              --  Host name resolution failed
      --  Certificate errors
      Certificate_Parse_Error,    --  Failed to parse certificate (bad DER format)
      Certificate_Verify_Failed,  --  Certificate verification failed
      Certificate_Expired,        --  Certificate has expired
      Certificate_Not_Yet_Valid,  --  Certificate not yet valid
      Certificate_Revoked,        --  Certificate revoked (CRL check)
      --  Private key errors
      Key_Parse_Error,        --  Failed to parse private key
      Key_Invalid_Format,     --  Invalid key format
      Key_Password_Required,  --  Password required for encrypted key
      --  Handshake errors
      Handshake_Timeout,      --  Handshake took too long
      Handshake_Failure,      --  Generic handshake failure
      Protocol_Version_Error, --  Incompatible TLS version
      Cipher_Suite_Error,     --  No common cipher suite
      Alert_Received,         --  Fatal alert from peer
      --  PSK errors
      PSK_Identity_Unknown,   --  Server doesn't know this PSK identity
      PSK_Key_Error,          --  PSK key mismatch
      --  Runtime errors
      Memory_Error,           --  Allocation failed
      RNG_Error,              --  Random number generator failed
      Internal_Error);        --  Unexpected internal error

   --  Get current connection state
   function State (Connection : TLS_Connection) return Connection_State;

   --  Get detailed error information after a failed operation
   --  Returns the error category for the last failed operation
   function Last_Error (Connection : TLS_Connection) return TLS_Error_Category;

   --  Get the raw mbedTLS error code for advanced debugging
   --  Returns 0 if no error
   function Last_Error_Code (Connection : TLS_Connection) return Integer;

   -----------------------
   --  Client Functions --
   -----------------------

   --  Connect to a Modbus/TCP Security server
   procedure Connect
     (Connection : in out TLS_Connection;
      Host       : String;
      Port       : Natural := Default_TLS_Port;
      Config     : TLS_Config;
      Timeout_Ms : Natural := 5000;
      Result     : out Status)
     with Pre  => Host'Length > 0 and Host'Length <= Max_Hostname_Length,
          Post => (if Result = Success then State (Connection) = Connected);

   --  Disconnect from server
   procedure Disconnect (Connection : in out TLS_Connection)
     with Post => State (Connection) = Disconnected;

   -----------------------
   --  Server Functions --
   -----------------------

   type TLS_Server is limited private;

   --  Server state
   type Server_State is (Closed, Listening, Error);

   --  Get current server state
   function State (Server : TLS_Server) return Server_State;

   --  Start listening for TLS connections
   --  Config must include Certificate and Private_Key (required for servers)
   procedure Listen
     (Server     : in out TLS_Server;
      Port       : Natural := Default_TLS_Port;
      Config     : TLS_Config;
      Result     : out Status)
     with Post => (if Result = Success then State (Server) = Listening);

   --  Accept a pending client connection
   --  Blocks until a client connects or timeout expires
   --  The Connection is initialized and ready for Send_Frame/Receive_Frame
   procedure Accept_Connection
     (Server     : in out TLS_Server;
      Connection : in out TLS_Connection;
      Timeout_Ms : Natural := 5000;
      Result     : out Status);

   --  Stop listening and close the server socket
   procedure Close_Server (Server : in out TLS_Server)
     with Post => State (Server) = Closed;

   -----------------
   --  Frame I/O  --
   -----------------

   --  Send a complete Modbus frame (includes MBAP header)
   procedure Send_Frame
     (Connection : in out TLS_Connection;
      Frame      : Byte_Array;
      Result     : out Status)
     with Pre => State (Connection) = Connected and Frame'Length > 0;

   --  Receive a complete Modbus frame
   --  Frame buffer should be at least Max_ADU_Size (260 bytes)
   procedure Receive_Frame
     (Connection : in out TLS_Connection;
      Frame      : out Byte_Array;
      Length     : out Natural;
      Timeout_Ms : Natural := 1000;
      Result     : out Status)
     with Pre  => State (Connection) = Connected and Frame'Length >= 260,  --  Max TCP ADU
          Post => (if Result = Success then Length <= Frame'Length);

private

   --  Internal buffer sizes (must match mbedTLS context sizes)
   --  Sized for embedded mbedTLS configurations (minimal config)
   --  Desktop mbedTLS 3.x has larger structures - use stubs for testing
   SSL_Context_Size    : constant := 512;
   SSL_Config_Size     : constant := 256;
   Entropy_Size        : constant := 1024;
   CTR_DRBG_Size       : constant := 512;
   X509_Crt_Size       : constant := 2048;
   PK_Context_Size     : constant := 256;
   Net_Context_Size    : constant := 8;
   SSL_Session_Size    : constant := 256;

   --  Opaque storage for mbedTLS contexts (avoid importing in spec)
   type Context_Storage is array (Natural range <>) of Byte
     with Convention => C;

   type TLS_Connection is limited record
      --  mbedTLS context storage (statically allocated)
      SSL_Ctx       : Context_Storage (1 .. SSL_Context_Size);
      SSL_Conf      : Context_Storage (1 .. SSL_Config_Size);
      Entropy       : Context_Storage (1 .. Entropy_Size);
      CTR_DRBG      : Context_Storage (1 .. CTR_DRBG_Size);
      CA_Cert       : Context_Storage (1 .. X509_Crt_Size);
      Own_Cert      : Context_Storage (1 .. X509_Crt_Size);
      Private_Key   : Context_Storage (1 .. PK_Context_Size);
      Net_Ctx       : Context_Storage (1 .. Net_Context_Size);

      --  Connection state
      Current_State : Connection_State := Disconnected;
      Initialized   : Boolean := False;

      --  Receive timeout (set during Connect, used in Receive_Frame)
      Read_Timeout_Ms : Natural := 1000;

      --  Server mode flag (for Accept_Connection)
      Is_Server_Side : Boolean := False;

      --  Error tracking for debugging
      Last_Error_Cat  : TLS_Error_Category := No_Error;
      Last_Mbed_Error : Integer := 0;

      --  Session resumption storage
      Pending_Session      : Context_Storage (1 .. SSL_Session_Size);
      Has_Pending_Session  : Boolean := False;
   end record;

   type TLS_Server is limited record
      --  Listening socket
      Listen_Ctx    : Context_Storage (1 .. Net_Context_Size);

      --  Shared configuration for all accepted connections
      SSL_Conf      : Context_Storage (1 .. SSL_Config_Size);
      Entropy       : Context_Storage (1 .. Entropy_Size);
      CTR_DRBG      : Context_Storage (1 .. CTR_DRBG_Size);
      Server_Cert   : Context_Storage (1 .. X509_Crt_Size);
      Server_Key    : Context_Storage (1 .. PK_Context_Size);
      CA_Cert       : Context_Storage (1 .. X509_Crt_Size);

      --  Server state
      Current_State : Server_State := Closed;
      Initialized   : Boolean := False;

      --  Config copy for accepted connections
      Stored_Config : TLS_Config;
   end record;

end Ada_Modbus.Transport.TLS_Mbed;
