--  Semihosting - ARM Semihosting support for QEMU
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides simple text output via ARM semihosting calls.
--  Works with QEMU when started with -semihosting flag.
--
--  Usage:
--    Semihosting.Put_Line ("Hello from embedded!");

with Interfaces; use Interfaces;

package Semihosting is

   --  Write a string to the debug console
   procedure Put (S : String);

   --  Write a string with newline
   procedure Put_Line (S : String);

   --  Write a single character
   procedure Put_Char (C : Character);

   --  Write a newline
   procedure New_Line;

   --  Write an unsigned integer (decimal)
   procedure Put (Value : Unsigned_32);

   --  Write an unsigned integer (hexadecimal, 4 digits)
   procedure Put_Hex (Value : Unsigned_16);

   --  Write an unsigned integer (hexadecimal, 8 digits)
   procedure Put_Hex (Value : Unsigned_32);

end Semihosting;
