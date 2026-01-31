--  mbedTLS Ada Bindings - Root Package
--  Thin bindings to the mbedTLS C library for embedded TLS support.
--
--  SPDX-License-Identifier: MIT

with Interfaces.C;

package MbedTLS is

   pragma Preelaborate;

   use Interfaces.C;

   --  Common types used across mbedTLS
   subtype Error_Code is int;

   --  Success and common error codes
   Success : constant Error_Code := 0;

   --  SSL/TLS error codes (from mbedtls/ssl.h)
   ERR_SSL_WANT_READ              : constant Error_Code := -16#6900#;
   ERR_SSL_WANT_WRITE             : constant Error_Code := -16#6880#;
   ERR_SSL_TIMEOUT                : constant Error_Code := -16#6800#;
   ERR_SSL_PEER_CLOSE_NOTIFY      : constant Error_Code := -16#7880#;
   ERR_SSL_CONN_EOF               : constant Error_Code := -16#7280#;
   ERR_SSL_NO_CLIENT_CERTIFICATE  : constant Error_Code := -16#7480#;
   ERR_SSL_CERTIFICATE_REQUIRED   : constant Error_Code := -16#7580#;
   ERR_SSL_CERTIFICATE_VERIFY     : constant Error_Code := -16#7D00#;
   ERR_SSL_BAD_CERTIFICATE        : constant Error_Code := -16#7A00#;
   ERR_SSL_HANDSHAKE_FAILURE      : constant Error_Code := -16#6E00#;
   ERR_SSL_FATAL_ALERT_MESSAGE    : constant Error_Code := -16#7780#;
   ERR_SSL_UNEXPECTED_MESSAGE     : constant Error_Code := -16#7700#;
   ERR_SSL_INVALID_RECORD         : constant Error_Code := -16#7200#;
   ERR_SSL_BUFFER_TOO_SMALL       : constant Error_Code := -16#6A00#;
   ERR_SSL_ALLOC_FAILED           : constant Error_Code := -16#7F00#;

   --  X.509 error codes
   ERR_X509_CERT_VERIFY_FAILED    : constant Error_Code := -16#2700#;
   ERR_X509_CERT_UNKNOWN_FORMAT   : constant Error_Code := -16#2180#;
   ERR_X509_BAD_INPUT_DATA        : constant Error_Code := -16#2100#;
   ERR_X509_INVALID_FORMAT        : constant Error_Code := -16#2400#;
   ERR_X509_INVALID_VERSION       : constant Error_Code := -16#2480#;
   ERR_X509_INVALID_SERIAL        : constant Error_Code := -16#2500#;
   ERR_X509_INVALID_ALG           : constant Error_Code := -16#2580#;
   ERR_X509_INVALID_NAME          : constant Error_Code := -16#2600#;
   ERR_X509_INVALID_DATE          : constant Error_Code := -16#2680#;
   ERR_X509_INVALID_SIGNATURE     : constant Error_Code := -16#2800#;
   ERR_X509_INVALID_EXTENSIONS    : constant Error_Code := -16#2900#;
   ERR_X509_CERT_EXPIRED          : constant Error_Code := -16#2A00#;
   ERR_X509_CERT_NOT_YET_VALID    : constant Error_Code := -16#2A80#;

   --  PK error codes
   ERR_PK_BAD_INPUT_DATA          : constant Error_Code := -16#3E80#;
   ERR_PK_KEY_INVALID_FORMAT      : constant Error_Code := -16#3D00#;
   ERR_PK_INVALID_PUBKEY          : constant Error_Code := -16#3B00#;
   ERR_PK_INVALID_ALG             : constant Error_Code := -16#3A80#;
   ERR_PK_PASSWORD_REQUIRED       : constant Error_Code := -16#3980#;
   ERR_PK_PASSWORD_MISMATCH       : constant Error_Code := -16#3900#;

   --  Net error codes
   ERR_NET_SOCKET_FAILED          : constant Error_Code := -16#0042#;
   ERR_NET_CONNECT_FAILED         : constant Error_Code := -16#0044#;
   ERR_NET_BIND_FAILED            : constant Error_Code := -16#0046#;
   ERR_NET_LISTEN_FAILED          : constant Error_Code := -16#0048#;
   ERR_NET_ACCEPT_FAILED          : constant Error_Code := -16#004A#;
   ERR_NET_CONN_RESET             : constant Error_Code := -16#004E#;
   ERR_NET_SEND_FAILED            : constant Error_Code := -16#004D#;
   ERR_NET_RECV_FAILED            : constant Error_Code := -16#004C#;
   ERR_NET_UNKNOWN_HOST           : constant Error_Code := -16#0052#;

   --  CTR_DRBG error codes
   ERR_CTR_DRBG_ENTROPY_SOURCE    : constant Error_Code := -16#0034#;
   ERR_CTR_DRBG_REQUEST_TOO_BIG   : constant Error_Code := -16#0036#;
   ERR_CTR_DRBG_INPUT_TOO_BIG     : constant Error_Code := -16#0038#;

   --  SSL endpoint types
   SSL_IS_CLIENT : constant int := 0;
   SSL_IS_SERVER : constant int := 1;

   --  SSL transport types
   SSL_TRANSPORT_STREAM   : constant int := 0;
   SSL_TRANSPORT_DATAGRAM : constant int := 1;

   --  SSL preset types
   SSL_PRESET_DEFAULT : constant int := 0;
   SSL_PRESET_SUITEB  : constant int := 2;

   --  SSL verification modes
   SSL_VERIFY_NONE     : constant int := 0;
   SSL_VERIFY_OPTIONAL : constant int := 1;
   SSL_VERIFY_REQUIRED : constant int := 2;

   --  Check if an error code indicates the operation should be retried
   function Is_Want_Read (Err : Error_Code) return Boolean is
     (Err = ERR_SSL_WANT_READ);

   function Is_Want_Write (Err : Error_Code) return Boolean is
     (Err = ERR_SSL_WANT_WRITE);

   function Is_Retriable (Err : Error_Code) return Boolean is
     (Err = ERR_SSL_WANT_READ or else Err = ERR_SSL_WANT_WRITE);

end MbedTLS;
