--  Test mbedTLS Bindings - Desktop verification
--  SPDX-License-Identifier: MIT
--
--  Tests for mbedTLS bindings to verify they compile and link correctly.
--  Run on desktop with libmbedtls-dev installed.
--
--  Build and run:
--    cd tls_mbed/tests && alr build && ./bin/test_bindings

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces.C; use Interfaces.C;
with System; use System;
with MbedTLS;
with MbedTLS.Entropy;
with MbedTLS.CTR_DRBG;
with MbedTLS.SSL;
with MbedTLS.X509;
with MbedTLS.PK;
with MbedTLS.Net;
with Ada_Modbus.Transport.TLS_Mbed;
with Ada_Modbus.Transport.TLS_Mbed.Hardware_RNG;
with Ada_Modbus.Transport.TLS_Mbed.Session;

procedure Test_Bindings is
   use MbedTLS;

   OK : constant Error_Code := MbedTLS.Success;
   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   procedure Test (Name : String; Passed : Boolean) is
   begin
      Test_Count := Test_Count + 1;
      Put (Name & "... ");
      if Passed then
         Put_Line ("PASS");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("FAIL");
      end if;
   end Test;

   --  Use access types to allocate contexts on heap instead of stack
   --  This avoids stack overflow with large mbedTLS context structures

   type Entropy_Access is access MbedTLS.Entropy.Entropy_Context;
   type DRBG_Access is access MbedTLS.CTR_DRBG.CTR_DRBG_Context;
   type SSL_Context_Access is access MbedTLS.SSL.SSL_Context;
   type SSL_Config_Access is access MbedTLS.SSL.SSL_Config;
   type X509_Access is access MbedTLS.X509.X509_Crt;
   type PK_Access is access MbedTLS.PK.PK_Context;
   type Net_Access is access MbedTLS.Net.Net_Context;
   type Session_Access is access MbedTLS.SSL.SSL_Session;

   procedure Free is new Ada.Unchecked_Deallocation
     (MbedTLS.Entropy.Entropy_Context, Entropy_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (MbedTLS.CTR_DRBG.CTR_DRBG_Context, DRBG_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (MbedTLS.SSL.SSL_Context, SSL_Context_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (MbedTLS.SSL.SSL_Config, SSL_Config_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (MbedTLS.X509.X509_Crt, X509_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (MbedTLS.PK.PK_Context, PK_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (MbedTLS.Net.Net_Context, Net_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (MbedTLS.SSL.SSL_Session, Session_Access);

   --  Heap-allocated contexts
   Entropy_Ptr : Entropy_Access := new MbedTLS.Entropy.Entropy_Context;
   DRBG_Ptr    : DRBG_Access := new MbedTLS.CTR_DRBG.CTR_DRBG_Context;
   SSL_Ptr     : SSL_Context_Access := new MbedTLS.SSL.SSL_Context;
   Conf_Ptr    : SSL_Config_Access := new MbedTLS.SSL.SSL_Config;
   Cert_Ptr    : X509_Access := new MbedTLS.X509.X509_Crt;
   PK_Ptr      : PK_Access := new MbedTLS.PK.PK_Context;
   Net_Ptr     : Net_Access := new MbedTLS.Net.Net_Context;
   Session_Ptr : Session_Access := new MbedTLS.SSL.SSL_Session;

   Ret : Error_Code;

   --  TLS Transport types
   use Ada_Modbus.Transport.TLS_Mbed;

begin
   Put_Line ("=== mbedTLS Ada Bindings Test Suite ===");
   Put_Line ("");

   ---------------------------------
   --  Test Group 1: Base Types   --
   ---------------------------------
   Put_Line ("--- Entropy & CTR_DRBG Tests ---");

   --  Test: Initialize entropy
   MbedTLS.Entropy.Init (Entropy_Ptr.all);
   Test ("Initialize entropy context", True);

   --  Test: Initialize CTR_DRBG
   MbedTLS.CTR_DRBG.Init (DRBG_Ptr.all);
   Test ("Initialize CTR_DRBG context", True);

   --  Test: Seed CTR_DRBG
   Ret := MbedTLS.CTR_DRBG.Seed
     (DRBG_Ptr.all,
      MbedTLS.Entropy.Entropy_Func'Access,
      Entropy_Ptr.all'Address,
      System.Null_Address, 0);
   Test ("Seed CTR_DRBG with entropy", Ret = OK);

   Put_Line ("");

   ---------------------------------
   --  Test Group 2: SSL/TLS      --
   ---------------------------------
   Put_Line ("--- SSL/TLS Context Tests ---");

   --  Test: Initialize SSL context
   MbedTLS.SSL.Init (SSL_Ptr.all);
   Test ("Initialize SSL context", True);

   --  Test: Initialize SSL config
   MbedTLS.SSL.Config_Init (Conf_Ptr.all);
   Test ("Initialize SSL config", True);

   --  Test: Set config defaults (client, TLS, default preset)
   Ret := MbedTLS.SSL.Config_Defaults
     (Conf_Ptr.all, SSL_IS_CLIENT, SSL_TRANSPORT_STREAM, SSL_PRESET_DEFAULT);
   Test ("Set SSL config defaults (client)", Ret = OK);

   --  Test: Set config defaults (server)
   MbedTLS.SSL.Config_Init (Conf_Ptr.all);  -- Reinit
   Ret := MbedTLS.SSL.Config_Defaults
     (Conf_Ptr.all, SSL_IS_SERVER, SSL_TRANSPORT_STREAM, SSL_PRESET_DEFAULT);
   Test ("Set SSL config defaults (server)", Ret = OK);

   --  Test: Set read timeout
   MbedTLS.SSL.Conf_Read_Timeout (Conf_Ptr.all, 5000);
   Test ("Set read timeout", True);

   --  Test: Set auth mode
   MbedTLS.SSL.Conf_Authmode (Conf_Ptr.all, SSL_VERIFY_NONE);
   Test ("Set auth mode (verify none)", True);

   MbedTLS.SSL.Conf_Authmode (Conf_Ptr.all, SSL_VERIFY_OPTIONAL);
   Test ("Set auth mode (verify optional)", True);

   MbedTLS.SSL.Conf_Authmode (Conf_Ptr.all, SSL_VERIFY_REQUIRED);
   Test ("Set auth mode (verify required)", True);

   Put_Line ("");

   ---------------------------------
   --  Test Group 3: Session      --
   ---------------------------------
   Put_Line ("--- Session Resumption Tests ---");

   --  Test: Initialize session
   MbedTLS.SSL.Session_Init (Session_Ptr.all);
   Test ("Initialize SSL session", True);

   --  Test: Free session
   MbedTLS.SSL.Session_Free (Session_Ptr.all);
   Test ("Free SSL session", True);

   Put_Line ("");

   ---------------------------------
   --  Test Group 4: X509/PK      --
   ---------------------------------
   Put_Line ("--- X509 & PK Context Tests ---");

   --  Test: Initialize X509 certificate
   MbedTLS.X509.Init (Cert_Ptr.all);
   Test ("Initialize X509 certificate context", True);

   --  Test: Initialize PK context
   MbedTLS.PK.Init (PK_Ptr.all);
   Test ("Initialize PK context", True);

   Put_Line ("");

   ---------------------------------
   --  Test Group 5: Network      --
   ---------------------------------
   Put_Line ("--- Network Context Tests ---");

   --  Test: Initialize net context
   MbedTLS.Net.Init (Net_Ptr.all);
   Test ("Initialize net context", True);

   Put_Line ("");

   ---------------------------------
   --  Test Group 6: Hardware RNG --
   ---------------------------------
   Put_Line ("--- Hardware RNG API Tests ---");

   --  Test: Check no RNG registered initially
   Test ("No hardware RNG registered initially",
         not Hardware_RNG.Is_Hardware_RNG_Registered);

   --  Test: Register null callback should fail
   Test ("Register null callback fails",
         not Hardware_RNG.Register_Hardware_RNG (null));

   --  Test: Unregister when nothing registered
   Hardware_RNG.Unregister_Hardware_RNG;
   Test ("Unregister when nothing registered", True);

   Put_Line ("");

   ---------------------------------
   --  Test Group 7: Session API  --
   ---------------------------------
   Put_Line ("--- Session Save/Restore API Tests ---");

   declare
      Saved : Session.Saved_Session;
   begin
      --  Test: Session not valid initially
      Test ("New saved session is not valid",
            not Session.Is_Valid (Saved));

      --  Test: Clear session
      Session.Clear (Saved);
      Test ("Clear session works", not Session.Is_Valid (Saved));
   end;

   Put_Line ("");

   ---------------------------------
   --  Test Group 8: TLS Config   --
   ---------------------------------
   Put_Line ("--- TLS Config Type Tests ---");

   declare
      Config : TLS_Config;
   begin
      --  Test: Default config values
      Test ("Default auth mode is Certificate",
            Config.Mode = Auth_Certificate);
      Test ("Default verify peer is True",
            Config.Verify_Peer);
      Test ("Default certificate is null",
            Config.Certificate = System.Null_Address);
      Test ("Default PSK is null",
            Config.PSK = System.Null_Address);
   end;

   Put_Line ("");

   ---------------------------------
   --  Test Group 9: Error Cats   --
   ---------------------------------
   Put_Line ("--- Error Category Tests ---");

   declare
      Conn : TLS_Connection;
   begin
      --  Test: Initial error state
      Test ("Initial error category is No_Error",
            Last_Error (Conn) = No_Error);
      Test ("Initial error code is 0",
            Last_Error_Code (Conn) = 0);
      Test ("Initial state is Disconnected",
            State (Conn) = Disconnected);
   end;

   Put_Line ("");

   ---------------------------------
   --  Cleanup                     --
   ---------------------------------
   Put_Line ("--- Cleanup ---");

   MbedTLS.PK.Free (PK_Ptr.all);
   Test ("Free PK context", True);

   MbedTLS.X509.Free (Cert_Ptr.all);
   Test ("Free X509 context", True);

   MbedTLS.Net.Free (Net_Ptr.all);
   Test ("Free net context", True);

   MbedTLS.SSL.Free (SSL_Ptr.all);
   Test ("Free SSL context", True);

   MbedTLS.SSL.Config_Free (Conf_Ptr.all);
   Test ("Free SSL config", True);

   MbedTLS.CTR_DRBG.Free (DRBG_Ptr.all);
   Test ("Free CTR_DRBG context", True);

   MbedTLS.Entropy.Free (Entropy_Ptr.all);
   Test ("Free entropy context", True);

   --  Deallocate heap memory
   Free (Entropy_Ptr);
   Free (DRBG_Ptr);
   Free (SSL_Ptr);
   Free (Conf_Ptr);
   Free (Cert_Ptr);
   Free (PK_Ptr);
   Free (Net_Ptr);
   Free (Session_Ptr);

   Put_Line ("");

   ---------------------------------
   --  Summary                     --
   ---------------------------------
   Put_Line ("=== Test Summary ===");
   Put_Line ("Passed:" & Natural'Image (Pass_Count) &
             " / " & Natural'Image (Test_Count));

   if Pass_Count = Test_Count then
      Put_Line ("All tests PASSED!");
   else
      Put_Line ("Some tests FAILED.");
   end if;
end Test_Bindings;
