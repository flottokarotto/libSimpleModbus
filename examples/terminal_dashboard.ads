--  Terminal_Dashboard - ANSI Terminal Helper for Live Dashboards
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides ANSI escape code utilities for terminal dashboards:
--  - Cursor positioning
--  - Colors and styles
--  - Screen clearing
--  - Box drawing

package Terminal_Dashboard is

   --  ANSI Colors
   type Color is
     (Black, Red, Green, Yellow, Blue, Magenta, Cyan, White,
      Bright_Black, Bright_Red, Bright_Green, Bright_Yellow,
      Bright_Blue, Bright_Magenta, Bright_Cyan, Bright_White,
      Default);

   --  Text styles
   type Style is (Normal, Bold, Dim, Italic, Underline, Blink, Inverse);

   --  Screen operations
   procedure Clear_Screen;
   procedure Clear_Line;
   procedure Home;

   --  Cursor operations
   procedure Move_To (Row, Col : Positive);
   procedure Hide_Cursor;
   procedure Show_Cursor;

   --  Color and style
   procedure Set_Foreground (C : Color);
   procedure Set_Background (C : Color);
   procedure Set_Style (S : Style);
   procedure Reset_Style;

   --  Combined: move and print with color
   procedure Print
     (Row, Col : Positive;
      Text     : String;
      FG       : Color := Default;
      BG       : Color := Default;
      S        : Style := Normal);

   --  Print right-aligned in a field
   procedure Print_Right
     (Row, Col : Positive;
      Width    : Positive;
      Text     : String;
      FG       : Color := Default);

   --  Box drawing (single line)
   procedure Draw_Box
     (Top, Left     : Positive;
      Width, Height : Positive;
      Title         : String := "");

   --  Horizontal line
   procedure Draw_HLine (Row, Col : Positive; Width : Positive);

   --  Vertical line
   procedure Draw_VLine (Row, Col : Positive; Height : Positive);

   --  Progress bar
   procedure Draw_Progress_Bar
     (Row, Col : Positive;
      Width    : Positive;
      Percent  : Float;
      FG       : Color := Green);

   --  Value formatting helpers
   function Fmt (Value : Float; Decimals : Natural := 1) return String;
   function Fmt_Power (Watts : Float) return String;
   function Fmt_Energy (Wh : Float) return String;

   --  Color coding for values
   function Power_Color (Watts : Float) return Color;
   function SOC_Color (Percent : Float) return Color;
   function Status_Color (Is_OK : Boolean) return Color;

end Terminal_Dashboard;
