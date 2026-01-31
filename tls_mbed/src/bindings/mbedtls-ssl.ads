--  mbedTLS Ada Bindings - SSL/TLS Functions
--  SPDX-License-Identifier: MIT

--  Note: Interfaces.C types are visible via parent package's use clause
with System;
with MbedTLS.X509;
with MbedTLS.PK;

package MbedTLS.SSL is

   pragma Preelaborate;

   --  Opaque SSL context
   type SSL_Context is limited private;

   --  Opaque SSL configuration
   type SSL_Config is limited private;

   --  BIO send callback type
   --  int (*f_send)(void *, const unsigned char *, size_t)
   type Send_Callback is access function
     (Ctx  : System.Address;
      Buf  : System.Address;
      Len  : size_t) return int
     with Convention => C;

   --  BIO receive callback type
   --  int (*f_recv)(void *, unsigned char *, size_t)
   type Recv_Callback is access function
     (Ctx  : System.Address;
      Buf  : System.Address;
      Len  : size_t) return int
     with Convention => C;

   --  BIO receive with timeout callback type
   --  int (*f_recv_timeout)(void *, unsigned char *, size_t, uint32_t)
   type Recv_Timeout_Callback is access function
     (Ctx     : System.Address;
      Buf     : System.Address;
      Len     : size_t;
      Timeout : unsigned) return int
     with Convention => C;

   --------------------
   --  SSL Context   --
   --------------------

   --  Initialize SSL context
   procedure Init (Ssl : out SSL_Context)
     with Import, Convention => C, External_Name => "mbedtls_ssl_init";

   --  Free SSL context
   procedure Free (Ssl : in out SSL_Context)
     with Import, Convention => C, External_Name => "mbedtls_ssl_free";

   --  Set up SSL context with configuration
   function Setup
     (Ssl  : in out SSL_Context;
      Conf : SSL_Config) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_ssl_setup";

   --  Set BIO callbacks
   procedure Set_Bio
     (Ssl          : in out SSL_Context;
      P_Bio        : System.Address;
      F_Send       : Send_Callback;
      F_Recv       : Recv_Callback;
      F_Recv_Timeo : Recv_Timeout_Callback)
     with Import, Convention => C, External_Name => "mbedtls_ssl_set_bio";

   --  Set hostname for SNI (Server Name Indication)
   function Set_Hostname
     (Ssl      : in out SSL_Context;
      Hostname : char_array) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_ssl_set_hostname";

   --  Perform TLS handshake
   function Handshake (Ssl : in out SSL_Context) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_ssl_handshake";

   --  Read data from SSL connection
   function Read
     (Ssl : in out SSL_Context;
      Buf : System.Address;
      Len : size_t) return int
     with Import, Convention => C, External_Name => "mbedtls_ssl_read";

   --  Write data to SSL connection
   function Write
     (Ssl : in out SSL_Context;
      Buf : System.Address;
      Len : size_t) return int
     with Import, Convention => C, External_Name => "mbedtls_ssl_write";

   --  Send close notify alert
   function Close_Notify (Ssl : in out SSL_Context) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_ssl_close_notify";

   --  Reset SSL context for reuse
   function Session_Reset (Ssl : in out SSL_Context) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_ssl_session_reset";

   --------------------
   --  SSL Config    --
   --------------------

   --  Initialize SSL configuration
   procedure Config_Init (Conf : out SSL_Config)
     with Import, Convention => C, External_Name => "mbedtls_ssl_config_init";

   --  Free SSL configuration
   procedure Config_Free (Conf : in out SSL_Config)
     with Import, Convention => C, External_Name => "mbedtls_ssl_config_free";

   --  Set SSL configuration defaults
   --  int mbedtls_ssl_config_defaults(mbedtls_ssl_config *conf,
   --                                   int endpoint, int transport, int preset)
   function Config_Defaults
     (Conf      : in out SSL_Config;
      Endpoint  : int;
      Transport : int;
      Preset    : int) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_ssl_config_defaults";

   --  Set RNG callback
   procedure Conf_Rng
     (Conf  : in out SSL_Config;
      F_Rng : System.Address;
      P_Rng : System.Address)
     with Import, Convention => C, External_Name => "mbedtls_ssl_conf_rng";

   --  Set certificate verification mode
   procedure Conf_Authmode
     (Conf     : in out SSL_Config;
      Authmode : int)
     with Import, Convention => C, External_Name => "mbedtls_ssl_conf_authmode";

   --  Set CA certificate chain for verification
   procedure Conf_Ca_Chain
     (Conf   : in out SSL_Config;
      Ca_Crt : in out X509.X509_Crt;
      Ca_Crl : System.Address)
     with Import, Convention => C, External_Name => "mbedtls_ssl_conf_ca_chain";

   --  Set own certificate and private key
   function Conf_Own_Cert
     (Conf    : in out SSL_Config;
      Own_Crt : in out X509.X509_Crt;
      Pk_Key  : in out PK.PK_Context) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_ssl_conf_own_cert";

   --  Set read timeout
   procedure Conf_Read_Timeout
     (Conf    : in out SSL_Config;
      Timeout : unsigned)
     with Import, Convention => C, External_Name => "mbedtls_ssl_conf_read_timeout";

   ---------------------------------
   --  Pre-Shared Key (PSK) API   --
   ---------------------------------

   --  Set the Pre-Shared Key (PSK) and identity for a client
   --  psk: pointer to the PSK data
   --  psk_len: length of PSK in bytes (typically 16-32)
   --  psk_identity: null-terminated identity string
   --  psk_identity_len: length of identity (without null terminator)
   function Conf_Psk
     (Conf            : in out SSL_Config;
      Psk             : System.Address;
      Psk_Len         : size_t;
      Psk_Identity    : System.Address;
      Psk_Identity_Len : size_t) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_ssl_conf_psk";

   --  PSK callback type for server-side PSK lookup
   --  int (*f_psk)(void *p_psk, mbedtls_ssl_context *ssl,
   --               const unsigned char *identity, size_t identity_len)
   --  Callback should call mbedtls_ssl_set_hs_psk() to set the PSK
   type PSK_Callback is access function
     (P_Psk        : System.Address;
      Ssl          : System.Address;
      Identity     : System.Address;
      Identity_Len : size_t) return int
     with Convention => C;

   --  Set PSK callback for server (to look up PSK by identity)
   procedure Conf_Psk_Cb
     (Conf  : in out SSL_Config;
      F_Psk : PSK_Callback;
      P_Psk : System.Address)
     with Import, Convention => C, External_Name => "mbedtls_ssl_conf_psk_cb";

   --  Set PSK for current handshake (called from PSK callback)
   function Set_Hs_Psk
     (Ssl     : in Out SSL_Context;
      Psk     : System.Address;
      Psk_Len : size_t) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_ssl_set_hs_psk";

   ---------------------------------
   --  Session Resumption API     --
   ---------------------------------

   --  Opaque SSL session type for session resumption
   type SSL_Session is limited private;

   --  Initialize session context
   procedure Session_Init (Session : out SSL_Session)
     with Import, Convention => C, External_Name => "mbedtls_ssl_session_init";

   --  Free session context
   procedure Session_Free (Session : in out SSL_Session)
     with Import, Convention => C, External_Name => "mbedtls_ssl_session_free";

   --  Save session data from an active connection
   --  Call after successful handshake to save session for later resumption
   --  Note: This performs a deep copy, the session can be saved after connection close
   function Get_Session
     (Ssl     : SSL_Context;
      Session : in out SSL_Session) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_ssl_get_session";

   --  Set session data for resumption
   --  Call before handshake to attempt session resumption
   --  The session is copied, so the original can be freed after this call
   function Set_Session
     (Ssl     : in Out SSL_Context;
      Session : SSL_Session) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_ssl_set_session";

   --  Session ticket configuration (for client)
   --  Enable or disable session ticket reception (client-side)
   --  use_tickets: 1 = enable (default), 0 = disable
   procedure Conf_Session_Tickets
     (Conf        : in Out SSL_Config;
      Use_Tickets : int)
     with Import, Convention => C, External_Name => "mbedtls_ssl_conf_session_tickets";

   --  Session ticket callback type for server
   --  Lifetime: recommended ticket lifetime in seconds
   type Session_Ticket_Write_Callback is access function
     (P_Ticket : System.Address;
      Session  : System.Address;
      Start    : System.Address;
      End_Ptr  : System.Address;
      Tlen     : access size_t;
      Lifetime : access unsigned) return int
     with Convention => C;

   type Session_Ticket_Parse_Callback is access function
     (P_Ticket : System.Address;
      Session  : System.Address;
      Buf      : System.Address;
      Len      : size_t) return int
     with Convention => C;

   --  Set session ticket callbacks for server
   procedure Conf_Session_Tickets_Cb
     (Conf    : in Out SSL_Config;
      F_Write : Session_Ticket_Write_Callback;
      F_Parse : Session_Ticket_Parse_Callback;
      P_Ticket : System.Address)
     with Import, Convention => C, External_Name => "mbedtls_ssl_conf_session_tickets_cb";

private

   --  Reserve enough space for the SSL context
   --  Sized for embedded mbedTLS (minimal config)
   type SSL_Context_Buffer is array (1 .. 512) of unsigned_char
     with Convention => C;

   type SSL_Context is limited record
      Buffer : SSL_Context_Buffer;
   end record
     with Convention => C;

   --  Reserve enough space for the SSL config
   --  Sized for embedded mbedTLS (minimal config)
   type SSL_Config_Buffer is array (1 .. 256) of unsigned_char
     with Convention => C;

   type SSL_Config is limited record
      Buffer : SSL_Config_Buffer;
   end record
     with Convention => C;

   --  Reserve enough space for the SSL session
   --  Session size depends on config, 256 bytes should be enough for basic session
   type SSL_Session_Buffer is array (1 .. 256) of unsigned_char
     with Convention => C;

   type SSL_Session is limited record
      Buffer : SSL_Session_Buffer;
   end record
     with Convention => C;

end MbedTLS.SSL;
