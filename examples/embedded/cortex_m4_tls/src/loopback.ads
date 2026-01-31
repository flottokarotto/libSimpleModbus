--  Loopback Test Support for QEMU
--  SPDX-License-Identifier: MIT
--
--  Provides Ada bindings to the loopback stub functions.

with Interfaces.C; use Interfaces.C;

package Loopback is

   pragma Preelaborate;

   --  Endpoint identifiers
   Client_Endpoint : constant := 0;
   Server_Endpoint : constant := 1;

   --  Reset all loopback buffers (call before test)
   procedure Reset
     with Import, Convention => C, External_Name => "loopback_reset";

   --  Set current endpoint for subsequent TLS operations
   --  Must be called before Send_Frame / Receive_Frame
   procedure Set_Endpoint (Endpoint : int)
     with Import, Convention => C, External_Name => "loopback_set_endpoint";

   --  Get buffer sizes for debugging
   function Client_To_Server_Count return size_t
     with Import, Convention => C, External_Name => "loopback_c2s_count";

   function Server_To_Client_Count return size_t
     with Import, Convention => C, External_Name => "loopback_s2c_count";

end Loopback;
