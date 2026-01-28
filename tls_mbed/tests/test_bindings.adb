--  Test mbedTLS Bindings - Desktop verification
--  SPDX-License-Identifier: MIT
--
--  Simple test to verify mbedTLS bindings compile and link correctly.
--  Run on desktop with libmbedtls-dev installed.

with Ada.Text_IO; use Ada.Text_IO;
with MbedTLS;
with MbedTLS.SSL;
with MbedTLS.Entropy;
with MbedTLS.CTR_DRBG;
with MbedTLS.X509;
with MbedTLS.PK;
with System;

procedure Test_Bindings is
   use MbedTLS;

   Entropy_Ctx : MbedTLS.Entropy.Entropy_Context;
   DRBG_Ctx    : MbedTLS.CTR_DRBG.CTR_DRBG_Context;
   Ret         : Error_Code;
begin
   Put_Line ("=== mbedTLS Bindings Test ===");
   Put_Line ("");

   --  Test 1: Initialize entropy
   Put ("Initializing entropy... ");
   MbedTLS.Entropy.Init (Entropy_Ctx);
   Put_Line ("OK");

   --  Test 2: Initialize CTR_DRBG
   Put ("Initializing CTR_DRBG... ");
   MbedTLS.CTR_DRBG.Init (DRBG_Ctx);
   Put_Line ("OK");

   --  Test 3: Seed CTR_DRBG
   Put ("Seeding CTR_DRBG... ");
   Ret := MbedTLS.CTR_DRBG.Seed
     (DRBG_Ctx,
      MbedTLS.Entropy.Entropy_Func'Access,
      Entropy_Ctx'Address,
      System.Null_Address, 0);

   if Ret = Success then
      Put_Line ("OK");
   else
      Put_Line ("FAILED (code:" & Error_Code'Image (Ret) & ")");
   end if;

   --  Clean up
   Put ("Cleaning up... ");
   MbedTLS.CTR_DRBG.Free (DRBG_Ctx);
   MbedTLS.Entropy.Free (Entropy_Ctx);
   Put_Line ("OK");

   Put_Line ("");
   if Ret = Success then
      Put_Line ("All tests PASSED");
   else
      Put_Line ("Some tests FAILED");
   end if;
end Test_Bindings;
