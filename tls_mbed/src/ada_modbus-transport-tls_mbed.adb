--  Ada_Modbus.Transport.TLS_Mbed - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Interfaces.C; use Interfaces.C;
with System.Address_To_Access_Conversions;
with MbedTLS.SSL;
with MbedTLS.Entropy;
with MbedTLS.CTR_DRBG;
with MbedTLS.X509;
with MbedTLS.PK;
with MbedTLS.Net;

package body Ada_Modbus.Transport.TLS_Mbed is

   use MbedTLS;

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

   -----------
   -- State --
   -----------

   function State (Connection : TLS_Connection) return Connection_State is
   begin
      return Connection.Current_State;
   end State;

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
      Host_C : char_array := To_C (Host);
      Port_Str : constant String := Natural'Image (Port);
      Port_C : char_array := To_C (Port_Str (Port_Str'First + 1 .. Port_Str'Last));
   begin
      Result := Success;
      Connection.Read_Timeout_Ms := Timeout_Ms;

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

      --  Seed the random number generator
      Ret := MbedTLS.CTR_DRBG.Seed
        (Get_DRBG (Connection).all,
         MbedTLS.Entropy.Entropy_Func'Access,
         Get_Entropy (Connection).all'Address,
         System.Null_Address, 0);

      if Ret /= Success then
         Connection.Current_State := Error;
         Result := Frame_Error;
         return;
      end if;

      --  Load CA certificate if provided
      if Config.CA_Certificate /= System.Null_Address and Config.CA_Cert_Len > 0 then
         Ret := MbedTLS.X509.Parse_DER
           (Get_CA_Cert (Connection).all,
            Config.CA_Certificate,
            size_t (Config.CA_Cert_Len));

         if Ret /= Success then
            Connection.Current_State := Error;
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

         if Ret /= Success then
            Connection.Current_State := Error;
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

            if Ret /= Success then
               Connection.Current_State := Error;
               Result := Frame_Error;
               return;
            end if;
         end if;
      end if;

      --  Set SSL configuration defaults (client, TLS, default preset)
      Ret := MbedTLS.SSL.Config_Defaults
        (Get_Conf (Connection).all,
         SSL_IS_CLIENT,
         SSL_TRANSPORT_STREAM,
         SSL_PRESET_DEFAULT);

      if Ret /= Success then
         Connection.Current_State := Error;
         Result := Frame_Error;
         return;
      end if;

      --  Configure verification mode
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

         if Ret /= Success then
            Connection.Current_State := Error;
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

      if Ret /= Success then
         Connection.Current_State := Error;
         Result := Frame_Error;
         return;
      end if;

      --  Set hostname for SNI
      Ret := MbedTLS.SSL.Set_Hostname (Get_SSL (Connection).all, Host_C);

      if Ret /= Success then
         Connection.Current_State := Error;
         Result := Frame_Error;
         return;
      end if;

      --  Connect TCP socket
      Ret := MbedTLS.Net.Connect
        (Get_Net (Connection).all, Host_C, Port_C, MbedTLS.Net.PROTO_TCP);

      if Ret /= Success then
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
         exit when Ret = Success;

         if not Is_Retriable (Ret) then
            Connection.Current_State := Handshake_Failed;
            Result := Frame_Error;
            return;
         end if;
      end loop;

      Connection.Current_State := Connected;
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

end Ada_Modbus.Transport.TLS_Mbed;
