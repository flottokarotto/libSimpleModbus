--  mbedTLS Ada Bindings - CTR_DRBG Random Generator
--  SPDX-License-Identifier: MIT

--  Note: Interfaces.C types are visible via parent package's use clause
with System;

package MbedTLS.CTR_DRBG is

   pragma Preelaborate;

   --  Opaque CTR_DRBG context
   type CTR_DRBG_Context is limited private;

   --  Entropy function callback type
   --  int (*f_entropy)(void *, unsigned char *, size_t)
   type Entropy_Callback is access function
     (Data   : System.Address;
      Output : System.Address;
      Len    : size_t) return int
     with Convention => C;

   --  Initialize CTR_DRBG context
   procedure Init (Ctx : out CTR_DRBG_Context)
     with Import, Convention => C, External_Name => "mbedtls_ctr_drbg_init";

   --  Free CTR_DRBG context
   procedure Free (Ctx : in out CTR_DRBG_Context)
     with Import, Convention => C, External_Name => "mbedtls_ctr_drbg_free";

   --  Seed the CTR_DRBG context
   --  int mbedtls_ctr_drbg_seed(mbedtls_ctr_drbg_context *ctx,
   --                            int (*f_entropy)(void *, unsigned char *, size_t),
   --                            void *p_entropy,
   --                            const unsigned char *custom,
   --                            size_t len)
   function Seed
     (Ctx        : in out CTR_DRBG_Context;
      F_Entropy  : Entropy_Callback;
      P_Entropy  : System.Address;
      Custom     : System.Address;
      Custom_Len : size_t) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_ctr_drbg_seed";

   --  Random generation function (for use as callback)
   --  int mbedtls_ctr_drbg_random(void *p_rng, unsigned char *output, size_t output_len)
   function Random
     (P_Rng      : System.Address;
      Output     : System.Address;
      Output_Len : size_t) return int
     with Import, Convention => C, External_Name => "mbedtls_ctr_drbg_random";

private

   --  Reserve enough space for the CTR_DRBG context
   --  Actual size ~320 bytes, use 512 for safety
   --  Sized for embedded mbedTLS (minimal config)
   type CTR_DRBG_Context_Buffer is array (1 .. 512) of unsigned_char
     with Convention => C;

   type CTR_DRBG_Context is limited record
      Buffer : CTR_DRBG_Context_Buffer;
   end record
     with Convention => C;

end MbedTLS.CTR_DRBG;
