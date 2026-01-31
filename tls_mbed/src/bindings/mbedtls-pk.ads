--  mbedTLS Ada Bindings - Private Key Handling
--  SPDX-License-Identifier: MIT

--  Note: Interfaces.C types are visible via parent package's use clause
with System;

package MbedTLS.PK is

   pragma Preelaborate;

   --  Opaque private key context
   type PK_Context is limited private;

   --  Initialize private key context
   procedure Init (Ctx : out PK_Context)
     with Import, Convention => C, External_Name => "mbedtls_pk_init";

   --  Free private key context
   procedure Free (Ctx : in out PK_Context)
     with Import, Convention => C, External_Name => "mbedtls_pk_free";

   --  Parse a private key in DER or PEM format
   --  int mbedtls_pk_parse_key(mbedtls_pk_context *ctx,
   --                           const unsigned char *key, size_t keylen,
   --                           const unsigned char *pwd, size_t pwdlen,
   --                           int (*f_rng)(void *, unsigned char *, size_t),
   --                           void *p_rng)
   function Parse_Key
     (Ctx     : in out PK_Context;
      Key     : System.Address;
      Keylen  : size_t;
      Pwd     : System.Address;
      Pwdlen  : size_t;
      F_Rng   : System.Address;
      P_Rng   : System.Address) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_pk_parse_key";

private

   --  Reserve enough space for the PK context
   --  Size varies by key type, use 256 bytes
   --  Sized for embedded mbedTLS (minimal config)
   type PK_Context_Buffer is array (1 .. 256) of unsigned_char
     with Convention => C;

   type PK_Context is limited record
      Buffer : PK_Context_Buffer;
   end record
     with Convention => C;

end MbedTLS.PK;
