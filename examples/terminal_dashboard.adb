--  Terminal_Dashboard - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Fixed;

package body Terminal_Dashboard is

   ESC : constant Character := Character'Val (27);

   --  Send ANSI escape sequence
   procedure Send (Code : String) is
   begin
      Put (ESC & "[" & Code);
   end Send;

   ------------------
   -- Clear_Screen --
   ------------------

   procedure Clear_Screen is
   begin
      Send ("2J");
      Home;
   end Clear_Screen;

   ----------------
   -- Clear_Line --
   ----------------

   procedure Clear_Line is
   begin
      Send ("2K");
   end Clear_Line;

   ----------
   -- Home --
   ----------

   procedure Home is
   begin
      Send ("H");
   end Home;

   -------------
   -- Move_To --
   -------------

   procedure Move_To (Row, Col : Positive) is
      Row_Str : constant String := Ada.Strings.Fixed.Trim (Row'Image, Ada.Strings.Left);
      Col_Str : constant String := Ada.Strings.Fixed.Trim (Col'Image, Ada.Strings.Left);
   begin
      Send (Row_Str & ";" & Col_Str & "H");
   end Move_To;

   -----------------
   -- Hide_Cursor --
   -----------------

   procedure Hide_Cursor is
   begin
      Send ("?25l");
   end Hide_Cursor;

   -----------------
   -- Show_Cursor --
   -----------------

   procedure Show_Cursor is
   begin
      Send ("?25h");
   end Show_Cursor;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground (C : Color) is
      Code : constant array (Color) of Natural :=
        [Black => 30, Red => 31, Green => 32, Yellow => 33,
         Blue => 34, Magenta => 35, Cyan => 36, White => 37,
         Bright_Black => 90, Bright_Red => 91, Bright_Green => 92,
         Bright_Yellow => 93, Bright_Blue => 94, Bright_Magenta => 95,
         Bright_Cyan => 96, Bright_White => 97, Default => 39];
      Code_Str : constant String := Ada.Strings.Fixed.Trim (Code (C)'Image, Ada.Strings.Left);
   begin
      Send (Code_Str & "m");
   end Set_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (C : Color) is
      Code : constant array (Color) of Natural :=
        [Black => 40, Red => 41, Green => 42, Yellow => 43,
         Blue => 44, Magenta => 45, Cyan => 46, White => 47,
         Bright_Black => 100, Bright_Red => 101, Bright_Green => 102,
         Bright_Yellow => 103, Bright_Blue => 104, Bright_Magenta => 105,
         Bright_Cyan => 106, Bright_White => 107, Default => 49];
      Code_Str : constant String := Ada.Strings.Fixed.Trim (Code (C)'Image, Ada.Strings.Left);
   begin
      Send (Code_Str & "m");
   end Set_Background;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style (S : Style) is
      Code : constant array (Style) of Natural :=
        [Normal => 0, Bold => 1, Dim => 2, Italic => 3,
         Underline => 4, Blink => 5, Inverse => 7];
      Code_Str : constant String := Ada.Strings.Fixed.Trim (Code (S)'Image, Ada.Strings.Left);
   begin
      Send (Code_Str & "m");
   end Set_Style;

   -----------------
   -- Reset_Style --
   -----------------

   procedure Reset_Style is
   begin
      Send ("0m");
   end Reset_Style;

   -----------
   -- Print --
   -----------

   procedure Print
     (Row, Col : Positive;
      Text     : String;
      FG       : Color := Default;
      BG       : Color := Default;
      S        : Style := Normal)
   is
   begin
      Move_To (Row, Col);
      if S /= Normal then
         Set_Style (S);
      end if;
      if FG /= Default then
         Set_Foreground (FG);
      end if;
      if BG /= Default then
         Set_Background (BG);
      end if;
      Put (Text);
      if FG /= Default or BG /= Default or S /= Normal then
         Reset_Style;
      end if;
   end Print;

   -----------------
   -- Print_Right --
   -----------------

   procedure Print_Right
     (Row, Col : Positive;
      Width    : Positive;
      Text     : String;
      FG       : Color := Default)
   is
      Padded : constant String (1 .. Width) := (others => ' ');
      Start  : constant Natural := Width - Text'Length + 1;
   begin
      Move_To (Row, Col);
      if FG /= Default then
         Set_Foreground (FG);
      end if;
      if Text'Length >= Width then
         Put (Text (Text'First .. Text'First + Width - 1));
      else
         Put (Padded (1 .. Start - 1));
         Put (Text);
      end if;
      if FG /= Default then
         Reset_Style;
      end if;
   end Print_Right;

   --------------
   -- Draw_Box --
   --------------

   procedure Draw_Box
     (Top, Left     : Positive;
      Width, Height : Positive;
      Title         : String := "")
   is
      --  Box drawing characters (Unicode)
      TL : constant String := "┌";
      TR : constant String := "┐";
      BL : constant String := "└";
      BR : constant String := "┘";
      H  : constant String := "─";
      V  : constant String := "│";
   begin
      --  Top border
      Move_To (Top, Left);
      Put (TL);
      for I in 1 .. Width - 2 loop
         Put (H);
      end loop;
      Put (TR);

      --  Title if provided
      if Title'Length > 0 and Title'Length < Width - 4 then
         Move_To (Top, Left + 2);
         Put (" " & Title & " ");
      end if;

      --  Sides
      for Row in Top + 1 .. Top + Height - 2 loop
         Move_To (Row, Left);
         Put (V);
         Move_To (Row, Left + Width - 1);
         Put (V);
      end loop;

      --  Bottom border
      Move_To (Top + Height - 1, Left);
      Put (BL);
      for I in 1 .. Width - 2 loop
         Put (H);
      end loop;
      Put (BR);
   end Draw_Box;

   ----------------
   -- Draw_HLine --
   ----------------

   procedure Draw_HLine (Row, Col : Positive; Width : Positive) is
      H : constant String := "─";
   begin
      Move_To (Row, Col);
      for I in 1 .. Width loop
         Put (H);
      end loop;
   end Draw_HLine;

   ----------------
   -- Draw_VLine --
   ----------------

   procedure Draw_VLine (Row, Col : Positive; Height : Positive) is
      V : constant String := "│";
   begin
      for R in Row .. Row + Height - 1 loop
         Move_To (R, Col);
         Put (V);
      end loop;
   end Draw_VLine;

   -----------------------
   -- Draw_Progress_Bar --
   -----------------------

   procedure Draw_Progress_Bar
     (Row, Col : Positive;
      Width    : Positive;
      Percent  : Float;
      FG       : Color := Green)
   is
      Fill_Char  : constant String := "█";
      Empty_Char : constant String := "░";
      Pct        : Float := Percent;
      Fill_Width : Natural;
   begin
      if Pct < 0.0 then
         Pct := 0.0;
      elsif Pct > 100.0 then
         Pct := 100.0;
      end if;

      Fill_Width := Natural (Pct * Float (Width) / 100.0);

      Move_To (Row, Col);
      Set_Foreground (FG);
      for I in 1 .. Fill_Width loop
         Put (Fill_Char);
      end loop;
      Set_Foreground (Bright_Black);
      for I in Fill_Width + 1 .. Width loop
         Put (Empty_Char);
      end loop;
      Reset_Style;
   end Draw_Progress_Bar;

   ---------
   -- Fmt --
   ---------

   function Fmt (Value : Float; Decimals : Natural := 1) return String is
      Result : String (1 .. 15);
   begin
      Ada.Float_Text_IO.Put (Result, Value, Aft => Decimals, Exp => 0);
      for I in Result'Range loop
         if Result (I) /= ' ' then
            return Result (I .. Result'Last);
         end if;
      end loop;
      return Result;
   end Fmt;

   ---------------
   -- Fmt_Power --
   ---------------

   function Fmt_Power (Watts : Float) return String is
      Abs_W : constant Float := abs Watts;
   begin
      if Abs_W >= 1_000_000.0 then
         return Fmt (Watts / 1_000_000.0, 2) & " MW";
      elsif Abs_W >= 1000.0 then
         return Fmt (Watts / 1000.0, 2) & " kW";
      else
         return Fmt (Watts, 0) & " W";
      end if;
   end Fmt_Power;

   ----------------
   -- Fmt_Energy --
   ----------------

   function Fmt_Energy (Wh : Float) return String is
      Abs_Wh : constant Float := abs Wh;
   begin
      if Abs_Wh >= 1_000_000.0 then
         return Fmt (Wh / 1_000_000.0, 2) & " MWh";
      elsif Abs_Wh >= 1000.0 then
         return Fmt (Wh / 1000.0, 2) & " kWh";
      else
         return Fmt (Wh, 0) & " Wh";
      end if;
   end Fmt_Energy;

   -----------------
   -- Power_Color --
   -----------------

   function Power_Color (Watts : Float) return Color is
   begin
      if Watts > 0.0 then
         return Green;       --  Producing/Exporting
      elsif Watts < 0.0 then
         return Yellow;      --  Consuming/Importing
      else
         return White;
      end if;
   end Power_Color;

   ---------------
   -- SOC_Color --
   ---------------

   function SOC_Color (Percent : Float) return Color is
   begin
      if Percent >= 80.0 then
         return Bright_Green;
      elsif Percent >= 50.0 then
         return Green;
      elsif Percent >= 20.0 then
         return Yellow;
      else
         return Red;
      end if;
   end SOC_Color;

   ------------------
   -- Status_Color --
   ------------------

   function Status_Color (Is_OK : Boolean) return Color is
   begin
      if Is_OK then
         return Green;
      else
         return Red;
      end if;
   end Status_Color;

end Terminal_Dashboard;
