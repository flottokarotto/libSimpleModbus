--  mbedTLS Ada Bindings - Entropy Source
--  SPDX-License-Identifier: MIT

--  Note: Interfaces.C types are visible via parent package's use clause
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

   ------------------------------------------
   --  Custom Entropy Source Registration  --
   ------------------------------------------

   --  Entropy source callback type
   --  int (*f_source)(void *data, unsigned char *output, size_t len, size_t *olen)
   --  Returns 0 on success, fills output buffer and sets olen to bytes written
   type Entropy_Source_Callback is access function
     (Data   : System.Address;
      Output : System.Address;
      Len    : size_t;
      Olen   : access size_t) return int
     with Convention => C;

   --  Source strength constants
   ENTROPY_SOURCE_STRONG : constant int := 1;  --  Cryptographically strong
   ENTROPY_SOURCE_WEAK   : constant int := 0;  --  Weak/non-cryptographic

   --  Add a custom entropy source
   --  f_source: callback function to gather entropy
   --  p_source: context pointer passed to callback
   --  threshold: minimum bytes to gather before considering source ready
   --  strong: ENTROPY_SOURCE_STRONG (1) or ENTROPY_SOURCE_WEAK (0)
   function Add_Source
     (Ctx       : in out Entropy_Context;
      F_Source  : Entropy_Source_Callback;
      P_Source  : System.Address;
      Threshold : size_t;
      Strong    : int) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_entropy_add_source";

private

   --  Reserve enough space for the entropy context
   --  Sized for embedded mbedTLS (minimal config), desktop needs more
   type Entropy_Context_Buffer is array (1 .. 1024) of unsigned_char
     with Convention => C;

   type Entropy_Context is limited record
      Buffer : Entropy_Context_Buffer;
   end record
     with Convention => C;

end MbedTLS.Entropy;
