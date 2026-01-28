--  mbedTLS Ada Bindings - Root Package
--  Thin bindings to the mbedTLS C library for embedded TLS support.
--
--  SPDX-License-Identifier: MIT

with Interfaces.C;
with System;

package MbedTLS is

   pragma Preelaborate;

   use Interfaces.C;

   --  Common types used across mbedTLS
   subtype Error_Code is int;

   --  Success and common error codes
   Success : constant Error_Code := 0;

   --  SSL/TLS error codes (from mbedtls/ssl.h)
   ERR_SSL_WANT_READ           : constant Error_Code := -16#6900#;
   ERR_SSL_WANT_WRITE          : constant Error_Code := -16#6880#;
   ERR_SSL_TIMEOUT             : constant Error_Code := -16#6800#;
   ERR_SSL_PEER_CLOSE_NOTIFY   : constant Error_Code := -16#7880#;
   ERR_SSL_CONN_EOF            : constant Error_Code := -16#7280#;

   --  X.509 error codes
   ERR_X509_CERT_VERIFY_FAILED : constant Error_Code := -16#2700#;

   --  Net error codes
   ERR_NET_CONN_RESET          : constant Error_Code := -16#004E#;
   ERR_NET_SEND_FAILED         : constant Error_Code := -16#004D#;
   ERR_NET_RECV_FAILED         : constant Error_Code := -16#004C#;

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
