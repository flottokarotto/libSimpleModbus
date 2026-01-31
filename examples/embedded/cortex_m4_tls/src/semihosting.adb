--  Semihosting - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Uses ARM semihosting SVC calls to communicate with debugger/QEMU.
--  Reference: ARM DUI 0471C "ARM Compiler Software Development Guide"

with System.Machine_Code;
with System;

package body Semihosting is

   --  Semihosting operation codes
   SYS_WRITEC : constant := 16#03#;  --  Write character

   --  Execute semihosting call
   --  On Cortex-M: BKPT #0xAB with R0=operation, R1=parameter
   procedure Semihost_Call (Op : Unsigned_32; Param : System.Address) is
   begin
      System.Machine_Code.Asm
        ("mov r0, %0" & ASCII.LF & ASCII.HT &
         "mov r1, %1" & ASCII.LF & ASCII.HT &
         "bkpt 0xAB",
         Inputs   => [Unsigned_32'Asm_Input ("r", Op),
                      System.Address'Asm_Input ("r", Param)],
         Volatile => True,
         Clobber  => "r0, r1, memory");
   end Semihost_Call;

   --------------
   -- Put_Char --
   --------------

   procedure Put_Char (C : Character) is
      Char_Addr : aliased Character := C;
   begin
      Semihost_Call (SYS_WRITEC, Char_Addr'Address);
   end Put_Char;

   ---------
   -- Put --
   ---------

   procedure Put (S : String) is
   begin
      for C of S loop
         Put_Char (C);
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String) is
   begin
      Put (S);
      New_Line;
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Put_Char (ASCII.LF);
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Value : Unsigned_32) is
      Buf : String (1 .. 10);
      Pos : Natural := Buf'Last;
      V   : Unsigned_32 := Value;
   begin
      if V = 0 then
         Put_Char ('0');
         return;
      end if;

      while V > 0 loop
         Buf (Pos) := Character'Val (Character'Pos ('0') + Natural (V mod 10));
         V := V / 10;
         Pos := Pos - 1;
      end loop;

      Put (Buf (Pos + 1 .. Buf'Last));
   end Put;

   -------------
   -- Put_Hex --
   -------------

   Hex_Chars : constant String := "0123456789ABCDEF";

   procedure Put_Hex (Value : Unsigned_16) is
   begin
      for I in reverse 0 .. 3 loop
         Put_Char (Hex_Chars (Natural (Shift_Right (Unsigned_32 (Value), I * 4) and 16#F#) + 1));
      end loop;
   end Put_Hex;

   procedure Put_Hex (Value : Unsigned_32) is
   begin
      for I in reverse 0 .. 7 loop
         Put_Char (Hex_Chars (Natural (Shift_Right (Value, I * 4) and 16#F#) + 1));
      end loop;
   end Put_Hex;

end Semihosting;
