--  mbedTLS Ada Bindings - Entropy Source
--  SPDX-License-Identifier: MIT

with Interfaces.C; use Interfaces.C;
with System;

package MbedTLS.Entropy is

   pragma Preelaborate;

   --  Opaque entropy context (size depends on mbedTLS config, use 1KB buffer)
   type Entropy_Context is limited private;

   --  Initialize entropy context
   procedure Init (Ctx : out Entropy_Context)
     with Import, Convention => C, External_Name => "mbedtls_entropy_init";

   --  Free entropy context
   procedure Free (Ctx : in out Entropy_Context)
     with Import, Convention => C, External_Name => "mbedtls_entropy_free";

   --  Entropy gathering function (for use as callback)
   --  int mbedtls_entropy_func(void *data, unsigned char *output, size_t len)
   function Entropy_Func
     (Data   : System.Address;
      Output : System.Address;
      Len    : size_t) return int
     with Import, Convention => C, External_Name => "mbedtls_entropy_func";

private

   --  Reserve enough space for the entropy context
   --  Actual size varies by config, 1024 bytes should be sufficient
   type Entropy_Context_Buffer is array (1 .. 1024) of unsigned_char
     with Convention => C;

   type Entropy_Context is limited record
      Buffer : Entropy_Context_Buffer;
   end record
     with Convention => C;

end MbedTLS.Entropy;
