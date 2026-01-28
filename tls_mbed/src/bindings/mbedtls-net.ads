--  mbedTLS Ada Bindings - Network I/O (Optional)
--  SPDX-License-Identifier: MIT
--
--  Note: For embedded systems using lwIP or similar, you may not need
--  mbedtls_net functions. Instead, provide custom BIO callbacks to
--  mbedtls_ssl_set_bio that wrap your platform's socket API.

with Interfaces.C; use Interfaces.C;
with System;

package MbedTLS.Net is

   pragma Preelaborate;

   --  Opaque network context
   type Net_Context is limited private;

   --  Protocol types
   PROTO_TCP : constant int := 0;
   PROTO_UDP : constant int := 1;

   --  Initialize network context
   procedure Init (Ctx : out Net_Context)
     with Import, Convention => C, External_Name => "mbedtls_net_init";

   --  Free network context
   procedure Free (Ctx : in out Net_Context)
     with Import, Convention => C, External_Name => "mbedtls_net_free";

   --  Connect to a host:port (client)
   function Connect
     (Ctx   : in out Net_Context;
      Host  : char_array;
      Port  : char_array;
      Proto : int) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_net_connect";

   --  Set socket to non-blocking mode
   function Set_Nonblock (Ctx : in out Net_Context) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_net_set_nonblock";

   --  Set socket to blocking mode
   function Set_Block (Ctx : in out Net_Context) return Error_Code
     with Import, Convention => C, External_Name => "mbedtls_net_set_block";

   --  Send data (use as BIO callback for SSL)
   function Send
     (Ctx : System.Address;
      Buf : System.Address;
      Len : size_t) return int
     with Import, Convention => C, External_Name => "mbedtls_net_send";

   --  Receive data (use as BIO callback for SSL)
   function Recv
     (Ctx : System.Address;
      Buf : System.Address;
      Len : size_t) return int
     with Import, Convention => C, External_Name => "mbedtls_net_recv";

   --  Receive data with timeout (use as BIO callback for SSL)
   function Recv_Timeout
     (Ctx     : System.Address;
      Buf     : System.Address;
      Len     : size_t;
      Timeout : unsigned) return int
     with Import, Convention => C, External_Name => "mbedtls_net_recv_timeout";

private

   --  Network context just holds a file descriptor
   type Net_Context is limited record
      Fd : int;
   end record
     with Convention => C;

end MbedTLS.Net;
