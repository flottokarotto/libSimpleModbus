--  Ada_Modbus.Energy.SunSpec.DER - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Interfaces;

package body Ada_Modbus.Energy.SunSpec.DER
  with SPARK_Mode => On
is

   -----------------------------------
   -- Decode_Control_Scale_Factors --
   -----------------------------------

   procedure Decode_Control_Scale_Factors
     (Regs : Register_Array;
      SF   : out DER_Control_Scale_Factors)
   is
      Base : constant Natural := Regs'First;
   begin
      SF.WLimPct_SF   := To_Scale_Factor (Regs (Base + Reg_704_WMaxLimPct_SF - 2));
      SF.WSet_SF      := To_Scale_Factor (Regs (Base + Reg_704_WSet_SF - 2));
      SF.WSetPct_SF   := To_Scale_Factor (Regs (Base + Reg_704_WSetPct_SF - 2));
      SF.VarSet_SF    := To_Scale_Factor (Regs (Base + Reg_704_VarSet_SF - 2));
      SF.VarSetPct_SF := To_Scale_Factor (Regs (Base + Reg_704_VarSetPct_SF - 2));
   end Decode_Control_Scale_Factors;

   -----------------------------
   -- Decode_Control_Settings --
   -----------------------------

   procedure Decode_Control_Settings
     (Regs     : Register_Array;
      SF       : DER_Control_Scale_Factors;
      Settings : out DER_Control_Settings)
   is
      use type Interfaces.Unsigned_32;
      Base : constant Natural := Regs'First;
      Tms_Hi, Tms_Lo : Register_Value;
      Timeout : Interfaces.Unsigned_32;
   begin
      --  Power limit enable
      if Regs (Base + Reg_WMaxLimPctEna - 2) = 1 then
         Settings.Power_Limit_Enabled := Enabled;
      else
         Settings.Power_Limit_Enabled := Disabled;
      end if;

      --  Power limit percentage
      Settings.Power_Limit_Pct := Apply_Scale
        (Regs (Base + Reg_WMaxLimPct - 2), SF.WLimPct_SF);

      --  Revert timeout (32-bit)
      Tms_Hi := Regs (Base + Reg_WMaxLimPctRvrtTms - 2);
      Tms_Lo := Regs (Base + Reg_WMaxLimPctRvrtTms - 1);
      Timeout := Interfaces.Shift_Left (Interfaces.Unsigned_32 (Tms_Hi), 16)
                 or Interfaces.Unsigned_32 (Tms_Lo);
      Settings.Power_Limit_Revert_S := Natural (Timeout mod 2**31);

      --  Var setpoint enable
      if Regs (Base + Reg_VarSetEna - 2) = 1 then
         Settings.Var_Setpoint_Enabled := Enabled;
      else
         Settings.Var_Setpoint_Enabled := Disabled;
      end if;

      --  Var setpoint percentage
      Settings.Var_Setpoint_Pct := Apply_Scale_Signed
        (Regs (Base + Reg_VarSetPct - 2), SF.VarSetPct_SF);

      --  Power ramp rate
      if Is_Implemented (Regs (Base + Reg_WRmp - 2)) then
         Settings.Power_Ramp_Pct_Sec := Float (Regs (Base + Reg_WRmp - 2));
      else
         Settings.Power_Ramp_Pct_Sec := 0.0;
      end if;
   end Decode_Control_Settings;

   ----------------------------------
   -- Decode_Status_Scale_Factors --
   ----------------------------------

   procedure Decode_Status_Scale_Factors
     (Regs : Register_Array;
      SF   : out DER_Status_Scale_Factors)
   is
      Base : constant Natural := Regs'First;
   begin
      SF.W_SF   := To_Scale_Factor (Regs (Base + Reg_701_W_SF - 2));
      SF.VA_SF  := To_Scale_Factor (Regs (Base + Reg_701_VA_SF - 2));
      SF.Var_SF := To_Scale_Factor (Regs (Base + Reg_701_Var_SF - 2));
      SF.PF_SF  := To_Scale_Factor (Regs (Base + Reg_701_PF_SF - 2));
      SF.Hz_SF  := To_Scale_Factor (Regs (Base + Reg_701_Hz_SF - 2));
   end Decode_Status_Scale_Factors;

   -----------------------
   -- Decode_DER_Status --
   -----------------------

   procedure Decode_DER_Status
     (Regs   : Register_Array;
      SF     : DER_Status_Scale_Factors;
      Status : out DER_Status)
   is
      use type Interfaces.Unsigned_32;
      Base    : constant Natural := Regs'First;
      St_Val  : constant Natural := Natural (Regs (Base + Reg_701_St - 2));
      Con_Val : constant Natural := Natural (Regs (Base + Reg_701_ConnSt - 2));
      Hz_Hi, Hz_Lo : Register_Value;
      Hz_Raw  : Interfaces.Unsigned_32;
   begin
      --  Operating state
      Status.Operating_State := (case St_Val is
                                    when 1 => State_Off,
                                    when 2 => State_Sleeping,
                                    when 3 => State_Starting,
                                    when 4 => State_Running,
                                    when 5 => State_Throttled,
                                    when 6 => State_Shutting_Down,
                                    when 7 => State_Fault,
                                    when 8 => State_Standby,
                                    when others => State_Unknown);

      --  Connection state
      Status.Conn_State := (case Con_Val is
                                     when 1 => Disconnected,
                                     when 2 => Connected,
                                     when others => Unknown_Conn);

      --  Power values
      Status.Active_Power_W := Apply_Scale_Signed
        (Regs (Base + Reg_701_W - 2), SF.W_SF);
      Status.Reactive_Power_Var := Apply_Scale_Signed
        (Regs (Base + Reg_701_Var - 2), SF.Var_SF);
      Status.Power_Factor := Apply_Scale_Signed
        (Regs (Base + Reg_701_PF - 2), SF.PF_SF);

      --  Frequency (32-bit)
      Hz_Hi := Regs (Base + Reg_701_Hz - 2);
      Hz_Lo := Regs (Base + Reg_701_Hz - 1);
      Hz_Raw := Interfaces.Shift_Left (Interfaces.Unsigned_32 (Hz_Hi), 16)
                or Interfaces.Unsigned_32 (Hz_Lo);
      Status.Frequency_Hz := Float (Hz_Raw) * Scale_Multipliers (SF.Hz_SF);

      --  Throttle
      if Is_Implemented (Regs (Base + Reg_701_ThrotPct - 2)) then
         Status.Throttle_Pct := Float (Regs (Base + Reg_701_ThrotPct - 2));
         Status.Is_Throttled := Status.Throttle_Pct > 0.0;
      else
         Status.Throttle_Pct := 0.0;
         Status.Is_Throttled := False;
      end if;
   end Decode_DER_Status;

   -------------------------
   -- Encode_Power_Limit --
   -------------------------

   procedure Encode_Power_Limit
     (Limit_Pct : Float;
      Enabled   : Boolean;
      Regs      : out Register_Array)
   is
      Pct_Val : Register_Value;
   begin
      --  Initialize all registers first
      Regs := [others => 0];

      --  Clamp to 0-100%
      if Limit_Pct < 0.0 then
         Pct_Val := 0;
      elsif Limit_Pct > 100.0 then
         Pct_Val := 100;
      else
         Pct_Val := Register_Value (Limit_Pct);
      end if;

      --  Reg 0: Enable (1=enabled, 0=disabled)
      if Enabled then
         Regs (Regs'First) := 1;
      end if;
      --  (already 0 if disabled from initialization)

      --  Reg 1: Power limit percentage (assuming SF=0)
      Regs (Regs'First + 1) := Pct_Val;

      --  Reg 2: Revert value (same as limit for simplicity)
      Regs (Regs'First + 2) := Pct_Val;

      --  Reg 3: Enable revert (0 = disabled, already set)
   end Encode_Power_Limit;

end Ada_Modbus.Energy.SunSpec.DER;
