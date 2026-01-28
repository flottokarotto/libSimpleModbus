--  mbedTLS Ada Bindings - SSL/TLS Functions
--  SPDX-License-Identifier: MIT

with Interfaces.C; use Interfaces.C;
with System;
with MbedTLS.X509;
with MbedTLS.PK;
with MbedTLS.CTR_DRBG;

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

private

   --  Reserve enough space for the SSL context
   --  Actual size ~200 bytes without session cache
   type SSL_Context_Buffer is array (1 .. 512) of unsigned_char
     with Convention => C;

   type SSL_Context is limited record
      Buffer : SSL_Context_Buffer;
   end record
     with Convention => C;

   --  Reserve enough space for the SSL config
   --  Actual size ~150 bytes
   type SSL_Config_Buffer is array (1 .. 256) of unsigned_char
     with Convention => C;

   type SSL_Config is limited record
      Buffer : SSL_Config_Buffer;
   end record
     with Convention => C;

end MbedTLS.SSL;
