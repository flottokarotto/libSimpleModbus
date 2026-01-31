--  mbedTLS Ada Bindings - X.509 Certificate Handling
--  SPDX-License-Identifier: MIT

--  Note: Interfaces.C types are visible via parent package's use clause
with System;

package MbedTLS.X509 is

   pragma Preelaborate;

   --  Opaque X.509 certificate context
   type X509_Crt is limited private;

   --  Initialize certificate context
   procedure Init (Crt : out X509_Crt)
     with Import, Convention => C, External_Name => "mbedtls_x509_crt_init";

   --  Free certificate context
   procedure Free (Crt : in out X509_Crt)
     with Import, Convention => C, External_Name => "mbedtls_x509_crt_free";

   --  Parse one or more DER-encoded certificates
   --  int mbedtls_x509_crt_parse(mbedtls_x509_crt *chain,
   --                             const unsigned char *buf,
   --                             size_t buflen)
   function Parse
     (Chain  : in out X509_Crt;
      Buf    : System.Address;
      Buflen : size_t) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_x509_crt_parse";

   --  Parse a DER-encoded certificate (single cert, no PEM)
   --  int mbedtls_x509_crt_parse_der(mbedtls_x509_crt *chain,
   --                                  const unsigned char *buf,
   --                                  size_t buflen)
   function Parse_DER
     (Chain  : in out X509_Crt;
      Buf    : System.Address;
      Buflen : size_t) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_x509_crt_parse_der";

private

   --  Reserve enough space for the X.509 context
   --  Actual size varies, use 2KB for certificate chain support
   type X509_Crt_Buffer is array (1 .. 2048) of unsigned_char
     with Convention => C;

   type X509_Crt is limited record
      Buffer : X509_Crt_Buffer;
   end record
     with Convention => C;

end MbedTLS.X509;
