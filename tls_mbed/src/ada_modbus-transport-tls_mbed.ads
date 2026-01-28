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

   --  TLS configuration for embedded systems
   --  Certificates should be in DER format (binary, not PEM)
   type TLS_Config is record
      --  Client certificate (optional for client, required for mutual auth)
      Certificate     : System.Address := System.Null_Address;
      Certificate_Len : Natural := 0;

      --  Client private key
      Private_Key     : System.Address := System.Null_Address;
      Private_Key_Len : Natural := 0;

      --  CA certificate for server verification
      CA_Certificate  : System.Address := System.Null_Address;
      CA_Cert_Len     : Natural := 0;

      --  Enable peer certificate verification
      Verify_Peer     : Boolean := True;
   end record;

   Default_TLS_Port : constant := 802;  --  Modbus/TCP Security port

   --  Connection state
   type Connection_State is (Disconnected, Connected, Handshake_Failed, Error);

   --  Get current connection state
   function State (Connection : TLS_Connection) return Connection_State;

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
      Result     : out Status);

   --  Disconnect from server
   procedure Disconnect (Connection : in out TLS_Connection);

   -----------------
   --  Frame I/O  --
   -----------------

   --  Send a complete Modbus frame (includes MBAP header)
   procedure Send_Frame
     (Connection : in out TLS_Connection;
      Frame      : Byte_Array;
      Result     : out Status);

   --  Receive a complete Modbus frame
   --  Frame buffer should be at least Max_ADU_Size (260 bytes)
   procedure Receive_Frame
     (Connection : in out TLS_Connection;
      Frame      : out Byte_Array;
      Length     : out Natural;
      Timeout_Ms : Natural := 1000;
      Result     : out Status);

private

   --  Maximum sizes
   Max_Hostname_Length : constant := 64;

   --  Internal buffer sizes (must match mbedTLS context sizes)
   SSL_Context_Size    : constant := 512;
   SSL_Config_Size     : constant := 256;
   Entropy_Size        : constant := 1024;
   CTR_DRBG_Size       : constant := 512;
   X509_Crt_Size       : constant := 2048;
   PK_Context_Size     : constant := 256;
   Net_Context_Size    : constant := 8;

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
   end record;

end Ada_Modbus.Transport.TLS_Mbed;
