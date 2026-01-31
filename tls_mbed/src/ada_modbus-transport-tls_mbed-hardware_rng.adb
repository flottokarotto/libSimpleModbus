--  Ada_Modbus.Transport.TLS_Mbed.Hardware_RNG - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Transport.TLS_Mbed.Hardware_RNG is

   --  Global state for registered hardware RNG
   Registered_Callback : RNG_Callback := null;

   ---------------------------
   -- Register_Hardware_RNG --
   ---------------------------

   function Register_Hardware_RNG (Callback : RNG_Callback) return Boolean is
   begin
      if Callback = null then
         return False;
      end if;

      Registered_Callback := Callback;
      return True;
   end Register_Hardware_RNG;

   -------------------------------
   -- Is_Hardware_RNG_Registered --
   -------------------------------

   function Is_Hardware_RNG_Registered return Boolean is
   begin
      return Registered_Callback /= null;
   end Is_Hardware_RNG_Registered;

   -----------------------------
   -- Unregister_Hardware_RNG --
   -----------------------------

   procedure Unregister_Hardware_RNG is
   begin
      Registered_Callback := null;
   end Unregister_Hardware_RNG;

   -----------------------------
   -- Get_Registered_Callback --
   -----------------------------

   function Get_Registered_Callback return RNG_Callback is
   begin
      return Registered_Callback;
   end Get_Registered_Callback;

end Ada_Modbus.Transport.TLS_Mbed.Hardware_RNG;
