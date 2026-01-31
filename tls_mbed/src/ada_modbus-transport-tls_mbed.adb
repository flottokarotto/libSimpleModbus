--  Ada_Modbus.Transport.TLS_Mbed - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Interfaces.C; use Interfaces.C;
with System; use System;
with System.Address_To_Access_Conversions;
with MbedTLS.SSL;
with MbedTLS.Entropy;
with MbedTLS.CTR_DRBG;
with MbedTLS.X509;
with MbedTLS.PK;
with MbedTLS.Net;
with Ada_Modbus.Transport.TLS_Mbed.Hardware_RNG;

package body Ada_Modbus.Transport.TLS_Mbed is

   use MbedTLS;

   --  Alias for mbedTLS success to avoid confusion with Ada_Modbus.Status
   OK : constant Error_Code := MbedTLS.Success;

   --  Map mbedTLS error code to TLS_Error_Category
   function Categorize_Error (Err : Error_Code) return TLS_Error_Category is
   begin
      --  Success
      if Err = MbedTLS.Success then
         return No_Error;
      end if;

      --  Network errors
      if Err = ERR_NET_SOCKET_FAILED or
         Err = ERR_NET_BIND_FAILED or
         Err = ERR_NET_LISTEN_FAILED or
         Err = ERR_NET_ACCEPT_FAILED
      then
         return Socket_Error;
      end if;

      if Err = ERR_NET_CONNECT_FAILED then
         return Connect_Failed;
      end if;

      if Err = ERR_NET_UNKNOWN_HOST then
         return DNS_Error;
      end if;

      --  Certificate errors
      if Err = ERR_X509_CERT_UNKNOWN_FORMAT or
         Err = ERR_X509_BAD_INPUT_DATA or
         Err = ERR_X509_INVALID_FORMAT or
         Err = ERR_X509_INVALID_VERSION or
         Err = ERR_X509_INVALID_SERIAL or
         Err = ERR_X509_INVALID_ALG or
         Err = ERR_X509_INVALID_NAME or
         Err = ERR_X509_INVALID_SIGNATURE or
         Err = ERR_X509_INVALID_EXTENSIONS
      then
         return Certificate_Parse_Error;
      end if;

      if Err = ERR_X509_CERT_VERIFY_FAILED or
         Err = ERR_SSL_CERTIFICATE_VERIFY or
         Err = ERR_SSL_BAD_CERTIFICATE
      then
         return Certificate_Verify_Failed;
      end if;

      if Err = ERR_X509_CERT_EXPIRED then
         return Certificate_Expired;
      end if;

      if Err = ERR_X509_CERT_NOT_YET_VALID or Err = ERR_X509_INVALID_DATE then
         return Certificate_Not_Yet_Valid;
      end if;

      --  Key errors
      if Err = ERR_PK_BAD_INPUT_DATA or Err = ERR_PK_KEY_INVALID_FORMAT then
         return Key_Parse_Error;
      end if;

      if Err = ERR_PK_INVALID_PUBKEY or Err = ERR_PK_INVALID_ALG then
         return Key_Invalid_Format;
      end if;

      if Err = ERR_PK_PASSWORD_REQUIRED or Err = ERR_PK_PASSWORD_MISMATCH then
         return Key_Password_Required;
      end if;

      --  Handshake/SSL errors
      if Err = ERR_SSL_TIMEOUT then
         return Handshake_Timeout;
      end if;

      if Err = ERR_SSL_HANDSHAKE_FAILURE or
         Err = ERR_SSL_UNEXPECTED_MESSAGE or
         Err = ERR_SSL_INVALID_RECORD
      then
         return Handshake_Failure;
      end if;

      if Err = ERR_SSL_FATAL_ALERT_MESSAGE then
         return Alert_Received;
      end if;

      if Err = ERR_SSL_NO_CLIENT_CERTIFICATE or
         Err = ERR_SSL_CERTIFICATE_REQUIRED
      then
         return Certificate_Verify_Failed;
      end if;

      --  Memory/RNG errors
      if Err = ERR_SSL_ALLOC_FAILED or Err = ERR_SSL_BUFFER_TOO_SMALL then
         return Memory_Error;
      end if;

      if Err = ERR_CTR_DRBG_ENTROPY_SOURCE or
         Err = ERR_CTR_DRBG_REQUEST_TOO_BIG or
         Err = ERR_CTR_DRBG_INPUT_TOO_BIG
      then
         return RNG_Error;
      end if;

      --  Default: internal error
      return Internal_Error;
   end Categorize_Error;

   --  Helper: Set error state on connection
   procedure Set_Error
     (Connection : in out TLS_Connection;
      Err        : Error_Code;
      New_State  : Connection_State)
   is
   begin
      Connection.Current_State := New_State;
      Connection.Last_Mbed_Error := Integer (Err);
      Connection.Last_Error_Cat := Categorize_Error (Err);
   end Set_Error;

   --  C-compatible wrapper for hardware RNG callback
   --  This is called by mbedTLS entropy system
   function Hardware_RNG_Entropy_Callback
     (Data   : System.Address;
      Output : System.Address;
      Len    : size_t;
      Olen   : access size_t) return int
     with Convention => C;

   function Hardware_RNG_Entropy_Callback
     (Data   : System.Address;
      Output : System.Address;
      Len    : size_t;
      Olen   : access size_t) return int
   is
      pragma Unreferenced (Data);
      Callback : constant Hardware_RNG.RNG_Callback :=
         Hardware_RNG.Get_Registered_Callback;
      Actual_Len : Natural;
      Success : Boolean;
   begin
      if not Hardware_RNG.Is_Hardware_RNG_Registered then
         Olen.all := 0;
         return -1;  --  Error: no callback registered
      end if;

      Success := Callback (Output, Natural (Len), Actual_Len);

      if Success then
         Olen.all := size_t (Actual_Len);
         return 0;  --  Success
      else
         Olen.all := 0;
         return -1;  --  Error
      end if;
   end Hardware_RNG_Entropy_Callback;

   --  Helper: Register hardware RNG with entropy context if available
   procedure Setup_Entropy_Source
     (Entropy_Ctx : in out MbedTLS.Entropy.Entropy_Context;
      Result      : out Error_Code)
   is
   begin
      Result := MbedTLS.Success;

      --  If hardware RNG is registered, add it as entropy source
      if Hardware_RNG.Is_Hardware_RNG_Registered then
         Result := MbedTLS.Entropy.Add_Source
           (Entropy_Ctx,
            Hardware_RNG_Entropy_Callback'Access,
            System.Null_Address,  --  No context needed, we use global state
            32,  --  Require at least 32 bytes before ready
            MbedTLS.Entropy.ENTROPY_SOURCE_STRONG);
      end if;
   end Setup_Entropy_Source;

   --  Address conversions for passing context addresses to C
   package SSL_Addr is new System.Address_To_Access_Conversions
     (MbedTLS.SSL.SSL_Context);
   package Conf_Addr is new System.Address_To_Access_Conversions
     (MbedTLS.SSL.SSL_Config);
   package Entropy_Addr is new System.Address_To_Access_Conversions
     (MbedTLS.Entropy.Entropy_Context);
   package DRBG_Addr is new System.Address_To_Access_Conversions
     (MbedTLS.CTR_DRBG.CTR_DRBG_Context);
   package X509_Addr is new System.Address_To_Access_Conversions
     (MbedTLS.X509.X509_Crt);
   package PK_Addr is new System.Address_To_Access_Conversions
     (MbedTLS.PK.PK_Context);
   package Net_Addr is new System.Address_To_Access_Conversions
     (MbedTLS.Net.Net_Context);

   --  Helper: Get SSL context from connection storage
   function Get_SSL (Conn : in out TLS_Connection)
     return access MbedTLS.SSL.SSL_Context
   is (SSL_Addr.To_Pointer (Conn.SSL_Ctx'Address));

   function Get_Conf (Conn : in out TLS_Connection)
     return access MbedTLS.SSL.SSL_Config
   is (Conf_Addr.To_Pointer (Conn.SSL_Conf'Address));

   function Get_Entropy (Conn : in out TLS_Connection)
     return access MbedTLS.Entropy.Entropy_Context
   is (Entropy_Addr.To_Pointer (Conn.Entropy'Address));

   function Get_DRBG (Conn : in out TLS_Connection)
     return access MbedTLS.CTR_DRBG.CTR_DRBG_Context
   is (DRBG_Addr.To_Pointer (Conn.CTR_DRBG'Address));

   function Get_CA_Cert (Conn : in out TLS_Connection)
     return access MbedTLS.X509.X509_Crt
   is (X509_Addr.To_Pointer (Conn.CA_Cert'Address));

   function Get_Own_Cert (Conn : in out TLS_Connection)
     return access MbedTLS.X509.X509_Crt
   is (X509_Addr.To_Pointer (Conn.Own_Cert'Address));

   function Get_PK (Conn : in out TLS_Connection)
     return access MbedTLS.PK.PK_Context
   is (PK_Addr.To_Pointer (Conn.Private_Key'Address));

   function Get_Net (Conn : in out TLS_Connection)
     return access MbedTLS.Net.Net_Context
   is (Net_Addr.To_Pointer (Conn.Net_Ctx'Address));

   --  Server helper functions
   function Get_Listen_Net (Srv : in out TLS_Server)
     return access MbedTLS.Net.Net_Context
   is (Net_Addr.To_Pointer (Srv.Listen_Ctx'Address));

   function Get_Srv_Conf (Srv : in out TLS_Server)
     return access MbedTLS.SSL.SSL_Config
   is (Conf_Addr.To_Pointer (Srv.SSL_Conf'Address));

   function Get_Srv_Entropy (Srv : in out TLS_Server)
     return access MbedTLS.Entropy.Entropy_Context
   is (Entropy_Addr.To_Pointer (Srv.Entropy'Address));

   function Get_Srv_DRBG (Srv : in out TLS_Server)
     return access MbedTLS.CTR_DRBG.CTR_DRBG_Context
   is (DRBG_Addr.To_Pointer (Srv.CTR_DRBG'Address));

   function Get_Srv_Cert (Srv : in out TLS_Server)
     return access MbedTLS.X509.X509_Crt
   is (X509_Addr.To_Pointer (Srv.Server_Cert'Address));

   function Get_Srv_Key (Srv : in out TLS_Server)
     return access MbedTLS.PK.PK_Context
   is (PK_Addr.To_Pointer (Srv.Server_Key'Address));

   function Get_Srv_CA (Srv : in out TLS_Server)
     return access MbedTLS.X509.X509_Crt
   is (X509_Addr.To_Pointer (Srv.CA_Cert'Address));

   -----------
   -- State --
   -----------

   function State (Connection : TLS_Connection) return Connection_State is
   begin
      return Connection.Current_State;
   end State;

   function State (Server : TLS_Server) return Server_State is
   begin
      return Server.Current_State;
   end State;

   ----------------
   -- Last_Error --
   ----------------

   function Last_Error (Connection : TLS_Connection) return TLS_Error_Category is
   begin
      return Connection.Last_Error_Cat;
   end Last_Error;

   ---------------------
   -- Last_Error_Code --
   ---------------------

   function Last_Error_Code (Connection : TLS_Connection) return Integer is
   begin
      return Connection.Last_Mbed_Error;
   end Last_Error_Code;

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
      Ret : Error_Code;
      Host_C : constant char_array := To_C (Host);
      Port_Str : constant String := Natural'Image (Port);
      Port_C : constant char_array := To_C (Port_Str (Port_Str'First + 1 .. Port_Str'Last));
   begin
      Result := Success;
      Connection.Read_Timeout_Ms := Timeout_Ms;
      Connection.Last_Error_Cat := No_Error;
      Connection.Last_Mbed_Error := 0;

      --  Initialize all contexts
      MbedTLS.Net.Init (Get_Net (Connection).all);
      MbedTLS.SSL.Init (Get_SSL (Connection).all);
      MbedTLS.SSL.Config_Init (Get_Conf (Connection).all);
      MbedTLS.Entropy.Init (Get_Entropy (Connection).all);
      MbedTLS.CTR_DRBG.Init (Get_DRBG (Connection).all);
      MbedTLS.X509.Init (Get_CA_Cert (Connection).all);
      MbedTLS.X509.Init (Get_Own_Cert (Connection).all);
      MbedTLS.PK.Init (Get_PK (Connection).all);

      Connection.Initialized := True;

      --  Setup hardware RNG if registered
      Setup_Entropy_Source (Get_Entropy (Connection).all, Ret);
      if Ret /= OK then
         Set_Error (Connection, Ret, Error);
         Result := Frame_Error;
         return;
      end if;

      --  Seed the random number generator
      Ret := MbedTLS.CTR_DRBG.Seed
        (Get_DRBG (Connection).all,
         MbedTLS.Entropy.Entropy_Func'Access,
         Get_Entropy (Connection).all'Address,
         System.Null_Address, 0);

      if Ret /= OK then
         Set_Error (Connection, Ret, Error);
         Result := Frame_Error;
         return;
      end if;

      --  Load certificates for certificate-based authentication
      if Config.Mode = Auth_Certificate then
         --  Load CA certificate if provided
         if Config.CA_Certificate /= System.Null_Address and Config.CA_Cert_Len > 0 then
            Ret := MbedTLS.X509.Parse_DER
              (Get_CA_Cert (Connection).all,
               Config.CA_Certificate,
               size_t (Config.CA_Cert_Len));

            if Ret /= OK then
               Set_Error (Connection, Ret, Error);
               Result := Frame_Error;
               return;
            end if;
         end if;

         --  Load client certificate and key if provided (for mutual authentication)
         if Config.Certificate /= System.Null_Address and Config.Certificate_Len > 0 then
            Ret := MbedTLS.X509.Parse_DER
              (Get_Own_Cert (Connection).all,
               Config.Certificate,
               size_t (Config.Certificate_Len));

            if Ret /= OK then
               Set_Error (Connection, Ret, Error);
               Result := Frame_Error;
               return;
            end if;

            --  Load private key
            if Config.Private_Key /= System.Null_Address and Config.Private_Key_Len > 0 then
               Ret := MbedTLS.PK.Parse_Key
                 (Get_PK (Connection).all,
                  Config.Private_Key,
                  size_t (Config.Private_Key_Len),
                  System.Null_Address, 0,  --  No password
                  MbedTLS.CTR_DRBG.Random'Address,
                  Get_DRBG (Connection).all'Address);

               if Ret /= OK then
                  Connection.Current_State := Error;
                  Result := Frame_Error;
                  return;
               end if;
            end if;
         end if;
      end if;

      --  Set SSL configuration defaults (client, TLS, default preset)
      Ret := MbedTLS.SSL.Config_Defaults
        (Get_Conf (Connection).all,
         SSL_IS_CLIENT,
         SSL_TRANSPORT_STREAM,
         SSL_PRESET_DEFAULT);

      if Ret /= OK then
         Set_Error (Connection, Ret, Error);
         Result := Frame_Error;
         return;
      end if;

      --  Configure authentication based on mode
      if Config.Mode = Auth_Certificate then
         --  Certificate-based authentication
         if Config.Verify_Peer then
            MbedTLS.SSL.Conf_Authmode (Get_Conf (Connection).all, SSL_VERIFY_REQUIRED);
         else
            MbedTLS.SSL.Conf_Authmode (Get_Conf (Connection).all, SSL_VERIFY_NONE);
         end if;

         --  Set CA chain for verification
         if Config.CA_Certificate /= System.Null_Address then
            MbedTLS.SSL.Conf_Ca_Chain
              (Get_Conf (Connection).all,
               Get_CA_Cert (Connection).all,
               System.Null_Address);  --  No CRL
         end if;

         --  Set own certificate if provided
         if Config.Certificate /= System.Null_Address then
            Ret := MbedTLS.SSL.Conf_Own_Cert
              (Get_Conf (Connection).all,
               Get_Own_Cert (Connection).all,
               Get_PK (Connection).all);

            if Ret /= OK then
               Set_Error (Connection, Ret, Error);
               Result := Frame_Error;
               return;
            end if;
         end if;

      else
         --  PSK authentication mode
         MbedTLS.SSL.Conf_Authmode (Get_Conf (Connection).all, SSL_VERIFY_NONE);

         --  Configure PSK
         if Config.PSK /= System.Null_Address and Config.PSK_Len > 0 and
            Config.PSK_Identity /= System.Null_Address and Config.PSK_Identity_Len > 0
         then
            Ret := MbedTLS.SSL.Conf_Psk
              (Get_Conf (Connection).all,
               Config.PSK,
               size_t (Config.PSK_Len),
               Config.PSK_Identity,
               size_t (Config.PSK_Identity_Len));

            if Ret /= OK then
               Set_Error (Connection, Ret, Error);
               Result := Frame_Error;
               return;
            end if;
         else
            --  PSK mode but no PSK configured
            Connection.Current_State := Error;
            Result := Invalid_Request;
            return;
         end if;
      end if;

      --  Set RNG function
      MbedTLS.SSL.Conf_Rng
        (Get_Conf (Connection).all,
         MbedTLS.CTR_DRBG.Random'Address,
         Get_DRBG (Connection).all'Address);

      --  Set read timeout
      MbedTLS.SSL.Conf_Read_Timeout
        (Get_Conf (Connection).all, unsigned (Timeout_Ms));

      --  Setup SSL context with configuration
      Ret := MbedTLS.SSL.Setup (Get_SSL (Connection).all, Get_Conf (Connection).all);

      if Ret /= OK then
         Set_Error (Connection, Ret, Error);
         Result := Frame_Error;
         return;
      end if;

      --  Apply pending session for resumption if available
      if Connection.Has_Pending_Session then
         declare
            package Session_Addr is new System.Address_To_Access_Conversions
              (MbedTLS.SSL.SSL_Session);
            Session_Ptr : constant Session_Addr.Object_Pointer :=
               Session_Addr.To_Pointer (Connection.Pending_Session'Address);
         begin
            Ret := MbedTLS.SSL.Set_Session
              (Get_SSL (Connection).all, Session_Ptr.all);
            --  Clear pending session regardless of success (one-shot)
            Connection.Has_Pending_Session := False;
            --  If session restoration fails, continue with full handshake
            --  (don't treat as fatal error - server may not support resumption)
         end;
      end if;

      --  Set hostname for SNI
      Ret := MbedTLS.SSL.Set_Hostname (Get_SSL (Connection).all, Host_C);

      if Ret /= OK then
         Set_Error (Connection, Ret, Error);
         Result := Frame_Error;
         return;
      end if;

      --  Connect TCP socket
      Ret := MbedTLS.Net.Connect
        (Get_Net (Connection).all, Host_C, Port_C, MbedTLS.Net.PROTO_TCP);

      if Ret /= OK then
         Connection.Current_State := Error;
         Result := Timeout;  --  Connection failed
         return;
      end if;

      --  Set BIO callbacks (using mbedTLS net functions)
      MbedTLS.SSL.Set_Bio
        (Get_SSL (Connection).all,
         Get_Net (Connection).all'Address,
         MbedTLS.Net.Send'Access,
         MbedTLS.Net.Recv'Access,
         MbedTLS.Net.Recv_Timeout'Access);

      --  Perform TLS handshake
      loop
         Ret := MbedTLS.SSL.Handshake (Get_SSL (Connection).all);
         exit when Ret = OK;

         if not Is_Retriable (Ret) then
            Set_Error (Connection, Ret, Handshake_Failed);
            Result := Frame_Error;
            return;
         end if;
      end loop;

      Connection.Current_State := Connected;
      Connection.Last_Error_Cat := No_Error;
      Connection.Last_Mbed_Error := 0;
      Result := Success;
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Connection : in out TLS_Connection) is
      Ret : Error_Code;
      pragma Unreferenced (Ret);
   begin
      if Connection.Current_State = Connected then
         --  Send close notify (ignore errors)
         Ret := MbedTLS.SSL.Close_Notify (Get_SSL (Connection).all);
      end if;

      if Connection.Initialized then
         --  Free all contexts
         MbedTLS.Net.Free (Get_Net (Connection).all);
         MbedTLS.SSL.Free (Get_SSL (Connection).all);
         MbedTLS.SSL.Config_Free (Get_Conf (Connection).all);
         MbedTLS.X509.Free (Get_CA_Cert (Connection).all);
         MbedTLS.X509.Free (Get_Own_Cert (Connection).all);
         MbedTLS.PK.Free (Get_PK (Connection).all);
         MbedTLS.CTR_DRBG.Free (Get_DRBG (Connection).all);
         MbedTLS.Entropy.Free (Get_Entropy (Connection).all);

         Connection.Initialized := False;
      end if;

      Connection.Current_State := Disconnected;
   end Disconnect;

   ----------------
   -- Send_Frame --
   ----------------

   procedure Send_Frame
     (Connection : in out TLS_Connection;
      Frame      : Byte_Array;
      Result     : out Status)
   is
      Ret : int;
      Sent : Natural := 0;
      Remaining : Natural := Frame'Length;
   begin
      if Connection.Current_State /= Connected then
         Result := Frame_Error;
         return;
      end if;

      --  Send all data (handle partial writes)
      while Remaining > 0 loop
         Ret := MbedTLS.SSL.Write
           (Get_SSL (Connection).all,
            Frame (Frame'First + Sent)'Address,
            size_t (Remaining));

         if Ret < 0 then
            if Is_Retriable (Error_Code (Ret)) then
               --  Would block, retry
               null;
            else
               Connection.Current_State := Error;
               Result := Frame_Error;
               return;
            end if;
         else
            Sent := Sent + Natural (Ret);
            Remaining := Remaining - Natural (Ret);
         end if;
      end loop;

      Result := Success;
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
      pragma Unreferenced (Timeout_Ms);  --  Using configured timeout
      Ret : int;
      MBAP_Header_Size : constant := 7;  --  Modbus TCP header size
      Header : Byte_Array (1 .. MBAP_Header_Size);
      PDU_Length : Natural;
      Received : Natural;
   begin
      Length := 0;

      if Connection.Current_State /= Connected then
         Result := Frame_Error;
         return;
      end if;

      --  First, read MBAP header (7 bytes)
      Received := 0;
      while Received < MBAP_Header_Size loop
         Ret := MbedTLS.SSL.Read
           (Get_SSL (Connection).all,
            Header (Header'First + Received)'Address,
            size_t (MBAP_Header_Size - Received));

         if Ret < 0 then
            if Ret = int (ERR_SSL_TIMEOUT) then
               Result := Timeout;
               return;
            elsif Ret = int (ERR_SSL_PEER_CLOSE_NOTIFY) or
                  Ret = int (ERR_SSL_CONN_EOF) then
               Connection.Current_State := Disconnected;
               Result := Frame_Error;
               return;
            elsif Is_Retriable (Error_Code (Ret)) then
               --  Would block, retry
               null;
            else
               Connection.Current_State := Error;
               Result := Frame_Error;
               return;
            end if;
         elsif Ret = 0 then
            --  Connection closed
            Connection.Current_State := Disconnected;
            Result := Frame_Error;
            return;
         else
            Received := Received + Natural (Ret);
         end if;
      end loop;

      --  Parse PDU length from MBAP header (bytes 5-6, big-endian)
      PDU_Length := Natural (Header (5)) * 256 + Natural (Header (6));

      --  Validate length
      if PDU_Length < 1 or PDU_Length > 253 then
         Result := Frame_Error;
         return;
      end if;

      --  Check if frame buffer is large enough
      if Frame'Length < MBAP_Header_Size + PDU_Length - 1 then
         Result := Buffer_Too_Small;
         return;
      end if;

      --  Copy header to output
      Frame (Frame'First .. Frame'First + MBAP_Header_Size - 1) := Header;

      --  Read remaining PDU data (PDU_Length includes Unit ID already in header)
      Received := 0;
      declare
         PDU_Data_Len : constant Natural := PDU_Length - 1;  --  Minus Unit ID
      begin
         while Received < PDU_Data_Len loop
            Ret := MbedTLS.SSL.Read
              (Get_SSL (Connection).all,
               Frame (Frame'First + MBAP_Header_Size + Received)'Address,
               size_t (PDU_Data_Len - Received));

            if Ret < 0 then
               if Ret = int (ERR_SSL_TIMEOUT) then
                  Result := Timeout;
                  return;
               elsif Is_Retriable (Error_Code (Ret)) then
                  null;
               else
                  Connection.Current_State := Error;
                  Result := Frame_Error;
                  return;
               end if;
            elsif Ret = 0 then
               Connection.Current_State := Disconnected;
               Result := Frame_Error;
               return;
            else
               Received := Received + Natural (Ret);
            end if;
         end loop;

         Length := MBAP_Header_Size + PDU_Data_Len;
      end;

      Result := Success;
   end Receive_Frame;

   ------------
   -- Listen --
   ------------

   procedure Listen
     (Server     : in out TLS_Server;
      Port       : Natural := Default_TLS_Port;
      Config     : TLS_Config;
      Result     : out Status)
   is
      Ret : Error_Code;
      Port_Str : constant String := Natural'Image (Port);
      Port_C : constant char_array := To_C (Port_Str (Port_Str'First + 1 .. Port_Str'Last));
      Bind_Ip : constant char_array := To_C ("0.0.0.0");
   begin
      Result := Success;

      --  Validate configuration based on mode
      if Config.Mode = Auth_Certificate then
         --  Server certificate is required for certificate mode
         if Config.Certificate = System.Null_Address or Config.Certificate_Len = 0 then
            Result := Invalid_Request;
            return;
         end if;

         if Config.Private_Key = System.Null_Address or Config.Private_Key_Len = 0 then
            Result := Invalid_Request;
            return;
         end if;
      else
         --  PSK is required for PSK mode
         if Config.PSK = System.Null_Address or Config.PSK_Len = 0 then
            Result := Invalid_Request;
            return;
         end if;
         if Config.PSK_Identity = System.Null_Address or Config.PSK_Identity_Len = 0 then
            Result := Invalid_Request;
            return;
         end if;
      end if;

      --  Initialize all contexts
      MbedTLS.Net.Init (Get_Listen_Net (Server).all);
      MbedTLS.SSL.Config_Init (Get_Srv_Conf (Server).all);
      MbedTLS.Entropy.Init (Get_Srv_Entropy (Server).all);
      Setup_Entropy_Source (Get_Srv_Entropy (Server).all, Ret);
      if Ret /= OK then
         Server.Current_State := Error;
         Result := Frame_Error;
         return;
      end if;
      MbedTLS.CTR_DRBG.Init (Get_Srv_DRBG (Server).all);
      MbedTLS.X509.Init (Get_Srv_Cert (Server).all);
      MbedTLS.X509.Init (Get_Srv_CA (Server).all);
      MbedTLS.PK.Init (Get_Srv_Key (Server).all);

      Server.Initialized := True;
      Server.Stored_Config := Config;

      --  Seed the random number generator
      Ret := MbedTLS.CTR_DRBG.Seed
        (Get_Srv_DRBG (Server).all,
         MbedTLS.Entropy.Entropy_Func'Access,
         Get_Srv_Entropy (Server).all'Address,
         System.Null_Address, 0);

      if Ret /= OK then
         Server.Current_State := Error;
         Result := Frame_Error;
         return;
      end if;

      --  Set SSL configuration defaults (server, TLS, default preset)
      Ret := MbedTLS.SSL.Config_Defaults
        (Get_Srv_Conf (Server).all,
         SSL_IS_SERVER,
         SSL_TRANSPORT_STREAM,
         SSL_PRESET_DEFAULT);

      if Ret /= OK then
         Server.Current_State := Error;
         Result := Frame_Error;
         return;
      end if;

      --  Configure authentication based on mode
      if Config.Mode = Auth_Certificate then
         --  Load server certificate
         Ret := MbedTLS.X509.Parse_DER
           (Get_Srv_Cert (Server).all,
            Config.Certificate,
            size_t (Config.Certificate_Len));

         if Ret /= OK then
            Server.Current_State := Error;
            Result := Frame_Error;
            return;
         end if;

         --  Load server private key
         Ret := MbedTLS.PK.Parse_Key
           (Get_Srv_Key (Server).all,
            Config.Private_Key,
            size_t (Config.Private_Key_Len),
            System.Null_Address, 0,
            MbedTLS.CTR_DRBG.Random'Address,
            Get_Srv_DRBG (Server).all'Address);

         if Ret /= OK then
            Server.Current_State := Error;
            Result := Frame_Error;
            return;
         end if;

         --  Load CA certificate for client verification if provided
         if Config.CA_Certificate /= System.Null_Address and Config.CA_Cert_Len > 0 then
            Ret := MbedTLS.X509.Parse_DER
              (Get_Srv_CA (Server).all,
               Config.CA_Certificate,
               size_t (Config.CA_Cert_Len));

            if Ret /= OK then
               Server.Current_State := Error;
               Result := Frame_Error;
               return;
            end if;
         end if;

         --  Configure client certificate verification
         if Config.Verify_Peer and Config.CA_Certificate /= System.Null_Address then
            MbedTLS.SSL.Conf_Authmode (Get_Srv_Conf (Server).all, SSL_VERIFY_REQUIRED);
            MbedTLS.SSL.Conf_Ca_Chain
              (Get_Srv_Conf (Server).all,
               Get_Srv_CA (Server).all,
               System.Null_Address);
         else
            MbedTLS.SSL.Conf_Authmode (Get_Srv_Conf (Server).all, SSL_VERIFY_NONE);
         end if;

         --  Set server certificate
         Ret := MbedTLS.SSL.Conf_Own_Cert
           (Get_Srv_Conf (Server).all,
            Get_Srv_Cert (Server).all,
            Get_Srv_Key (Server).all);

         if Ret /= OK then
            Server.Current_State := Error;
            Result := Frame_Error;
            return;
         end if;

      else
         --  PSK mode - configure PSK
         MbedTLS.SSL.Conf_Authmode (Get_Srv_Conf (Server).all, SSL_VERIFY_NONE);

         Ret := MbedTLS.SSL.Conf_Psk
           (Get_Srv_Conf (Server).all,
            Config.PSK,
            size_t (Config.PSK_Len),
            Config.PSK_Identity,
            size_t (Config.PSK_Identity_Len));

         if Ret /= OK then
            Server.Current_State := Error;
            Result := Frame_Error;
            return;
         end if;
      end if;

      --  Set RNG function
      MbedTLS.SSL.Conf_Rng
        (Get_Srv_Conf (Server).all,
         MbedTLS.CTR_DRBG.Random'Address,
         Get_Srv_DRBG (Server).all'Address);

      --  Bind to port
      Ret := MbedTLS.Net.Bind
        (Get_Listen_Net (Server).all,
         Bind_Ip,
         Port_C,
         MbedTLS.Net.PROTO_TCP);

      if Ret /= OK then
         Server.Current_State := Error;
         Result := Frame_Error;
         return;
      end if;

      Server.Current_State := Listening;
      Result := Success;
   end Listen;

   -----------------------
   -- Accept_Connection --
   -----------------------

   procedure Accept_Connection
     (Server     : in out TLS_Server;
      Connection : in out TLS_Connection;
      Timeout_Ms : Natural := 5000;
      Result     : out Status)
   is
      Ret : Error_Code;
      Dummy_Ip_Len : aliased size_t := 0;
   begin
      Result := Success;

      if Server.Current_State /= Listening then
         Result := Frame_Error;
         return;
      end if;

      --  Initialize connection contexts
      MbedTLS.Net.Init (Get_Net (Connection).all);
      MbedTLS.SSL.Init (Get_SSL (Connection).all);
      MbedTLS.SSL.Config_Init (Get_Conf (Connection).all);
      MbedTLS.Entropy.Init (Get_Entropy (Connection).all);
      Setup_Entropy_Source (Get_Entropy (Connection).all, Ret);
      if Ret /= OK then
         Set_Error (Connection, Ret, Error);
         Result := Frame_Error;
         return;
      end if;
      MbedTLS.CTR_DRBG.Init (Get_DRBG (Connection).all);
      MbedTLS.X509.Init (Get_CA_Cert (Connection).all);
      MbedTLS.X509.Init (Get_Own_Cert (Connection).all);
      MbedTLS.PK.Init (Get_PK (Connection).all);

      Connection.Initialized := True;
      Connection.Is_Server_Side := True;
      Connection.Read_Timeout_Ms := Timeout_Ms;

      --  Seed connection's RNG
      Ret := MbedTLS.CTR_DRBG.Seed
        (Get_DRBG (Connection).all,
         MbedTLS.Entropy.Entropy_Func'Access,
         Get_Entropy (Connection).all'Address,
         System.Null_Address, 0);

      if Ret /= OK then
         Set_Error (Connection, Ret, Error);
         Result := Frame_Error;
         return;
      end if;

      --  Accept client connection
      Ret := MbedTLS.Net.Do_Accept
        (Get_Listen_Net (Server).all,
         Get_Net (Connection).all,
         System.Null_Address,
         0,
         Dummy_Ip_Len'Access);

      if Ret /= OK then
         Connection.Current_State := Error;
         Result := Timeout;
         return;
      end if;

      --  Setup SSL for this connection (server mode)
      Ret := MbedTLS.SSL.Config_Defaults
        (Get_Conf (Connection).all,
         SSL_IS_SERVER,
         SSL_TRANSPORT_STREAM,
         SSL_PRESET_DEFAULT);

      if Ret /= OK then
         Set_Error (Connection, Ret, Error);
         Result := Frame_Error;
         return;
      end if;

      --  Configure authentication based on mode
      if Server.Stored_Config.Mode = Auth_Certificate then
         --  Copy server certificate/key configuration to connection
         if Server.Stored_Config.Certificate /= System.Null_Address then
            Ret := MbedTLS.X509.Parse_DER
              (Get_Own_Cert (Connection).all,
               Server.Stored_Config.Certificate,
               size_t (Server.Stored_Config.Certificate_Len));

            if Ret /= OK then
               Set_Error (Connection, Ret, Error);
               Result := Frame_Error;
               return;
            end if;

            Ret := MbedTLS.PK.Parse_Key
              (Get_PK (Connection).all,
               Server.Stored_Config.Private_Key,
               size_t (Server.Stored_Config.Private_Key_Len),
               System.Null_Address, 0,
               MbedTLS.CTR_DRBG.Random'Address,
               Get_DRBG (Connection).all'Address);

            if Ret /= OK then
               Set_Error (Connection, Ret, Error);
               Result := Frame_Error;
               return;
            end if;

            Ret := MbedTLS.SSL.Conf_Own_Cert
              (Get_Conf (Connection).all,
               Get_Own_Cert (Connection).all,
               Get_PK (Connection).all);

            if Ret /= OK then
               Set_Error (Connection, Ret, Error);
               Result := Frame_Error;
               return;
            end if;
         end if;

         --  Configure client verification for this connection
         if Server.Stored_Config.Verify_Peer and
            Server.Stored_Config.CA_Certificate /= System.Null_Address
         then
            Ret := MbedTLS.X509.Parse_DER
              (Get_CA_Cert (Connection).all,
               Server.Stored_Config.CA_Certificate,
               size_t (Server.Stored_Config.CA_Cert_Len));

            if Ret = OK then
               MbedTLS.SSL.Conf_Authmode (Get_Conf (Connection).all, SSL_VERIFY_REQUIRED);
               MbedTLS.SSL.Conf_Ca_Chain
                 (Get_Conf (Connection).all,
                  Get_CA_Cert (Connection).all,
                  System.Null_Address);
            end if;
         else
            MbedTLS.SSL.Conf_Authmode (Get_Conf (Connection).all, SSL_VERIFY_NONE);
         end if;

      else
         --  PSK mode - configure PSK for this connection
         MbedTLS.SSL.Conf_Authmode (Get_Conf (Connection).all, SSL_VERIFY_NONE);

         Ret := MbedTLS.SSL.Conf_Psk
           (Get_Conf (Connection).all,
            Server.Stored_Config.PSK,
            size_t (Server.Stored_Config.PSK_Len),
            Server.Stored_Config.PSK_Identity,
            size_t (Server.Stored_Config.PSK_Identity_Len));

         if Ret /= OK then
            Set_Error (Connection, Ret, Error);
            Result := Frame_Error;
            return;
         end if;
      end if;

      --  Set RNG function
      MbedTLS.SSL.Conf_Rng
        (Get_Conf (Connection).all,
         MbedTLS.CTR_DRBG.Random'Address,
         Get_DRBG (Connection).all'Address);

      --  Set read timeout
      MbedTLS.SSL.Conf_Read_Timeout
        (Get_Conf (Connection).all, unsigned (Timeout_Ms));

      --  Setup SSL context with configuration
      Ret := MbedTLS.SSL.Setup (Get_SSL (Connection).all, Get_Conf (Connection).all);

      if Ret /= OK then
         Set_Error (Connection, Ret, Error);
         Result := Frame_Error;
         return;
      end if;

      --  Set BIO callbacks
      MbedTLS.SSL.Set_Bio
        (Get_SSL (Connection).all,
         Get_Net (Connection).all'Address,
         MbedTLS.Net.Send'Access,
         MbedTLS.Net.Recv'Access,
         MbedTLS.Net.Recv_Timeout'Access);

      --  Perform TLS handshake
      loop
         Ret := MbedTLS.SSL.Handshake (Get_SSL (Connection).all);
         exit when Ret = OK;

         if not Is_Retriable (Ret) then
            Set_Error (Connection, Ret, Handshake_Failed);
            Result := Frame_Error;
            return;
         end if;
      end loop;

      Connection.Current_State := Connected;
      Connection.Last_Error_Cat := No_Error;
      Connection.Last_Mbed_Error := 0;
      Result := Success;
   end Accept_Connection;

   ------------------
   -- Close_Server --
   ------------------

   procedure Close_Server (Server : in out TLS_Server) is
   begin
      if Server.Initialized then
         MbedTLS.Net.Free (Get_Listen_Net (Server).all);
         MbedTLS.SSL.Config_Free (Get_Srv_Conf (Server).all);
         MbedTLS.X509.Free (Get_Srv_Cert (Server).all);
         MbedTLS.X509.Free (Get_Srv_CA (Server).all);
         MbedTLS.PK.Free (Get_Srv_Key (Server).all);
         MbedTLS.CTR_DRBG.Free (Get_Srv_DRBG (Server).all);
         MbedTLS.Entropy.Free (Get_Srv_Entropy (Server).all);

         Server.Initialized := False;
      end if;

      Server.Current_State := Closed;
   end Close_Server;

end Ada_Modbus.Transport.TLS_Mbed;
