--  Ada_Modbus.Energy.SunSpec.Battery - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Interfaces;

package body Ada_Modbus.Energy.SunSpec.Battery
  with SPARK_Mode => On
is

   ----------------------------------
   -- Decode_Battery_Scale_Factors --
   ----------------------------------

   procedure Decode_Battery_Scale_Factors
     (Regs : Register_Array;
      SF   : out Battery_Scale_Factors)
   is
      Base : constant Natural := Regs'First;
   begin
      SF.Ah_SF    := To_Scale_Factor (Regs (Base + Reg_AHRtg_SF - 2));
      SF.Wh_SF    := To_Scale_Factor (Regs (Base + Reg_WHRtg_SF - 2));
      SF.WRate_SF := To_Scale_Factor (Regs (Base + Reg_WChaDisChaMax_SF - 2));
      SF.SoC_SF   := To_Scale_Factor (Regs (Base + Reg_SoC_SF - 2));
      SF.DoD_SF   := To_Scale_Factor (Regs (Base + Reg_DoD_SF - 2));
      SF.SoH_SF   := To_Scale_Factor (Regs (Base + Reg_SoH_SF - 2));
      SF.V_SF     := To_Scale_Factor (Regs (Base + Reg_V_SF - 2));
      SF.CellV_SF := To_Scale_Factor (Regs (Base + Reg_CellV_SF - 2));
      SF.A_SF     := To_Scale_Factor (Regs (Base + Reg_A_SF - 2));
      SF.W_SF     := To_Scale_Factor (Regs (Base + Reg_W_SF - 2));
   end Decode_Battery_Scale_Factors;

   -------------------------
   -- Decode_Battery_Data --
   -------------------------

   procedure Decode_Battery_Data
     (Regs : Register_Array;
      SF   : Battery_Scale_Factors;
      Data : out Battery_Data)
   is
      use type Interfaces.Unsigned_32;
      Base      : constant Natural := Regs'First;
      Type_Val  : constant Natural := Natural (Regs (Base + Reg_Typ - 2));
      State_Val : constant Natural := Natural (Regs (Base + Reg_State - 2));
      ChSt_Val  : constant Natural := Natural (Regs (Base + Reg_ChaSt - 2));
      NCyc_Hi, NCyc_Lo : Register_Value;
      Evt1_Hi   : Register_Value;
   begin
      --  Ratings
      Data.Capacity_Ah := Apply_Scale (Regs (Base + Reg_AHRtg - 2), SF.Ah_SF);
      Data.Capacity_Wh := Apply_Scale (Regs (Base + Reg_WHRtg - 2), SF.Wh_SF);
      Data.Max_Charge_W := Apply_Scale (Regs (Base + Reg_WChaRteMax - 2), SF.WRate_SF);
      Data.Max_Discharge_W := Apply_Scale (Regs (Base + Reg_WDisChaRteMax - 2), SF.WRate_SF);

      --  State of charge/health
      Data.SOC_Percent := Apply_Scale (Regs (Base + Reg_SoC - 2), SF.SoC_SF);

      if Is_Implemented (Regs (Base + Reg_DoD - 2)) then
         Data.DOD_Percent := Apply_Scale (Regs (Base + Reg_DoD - 2), SF.DoD_SF);
      else
         Data.DOD_Percent := 100.0 - Data.SOC_Percent;
      end if;

      if Is_Implemented (Regs (Base + Reg_SoH - 2)) then
         Data.SOH_Percent := Apply_Scale (Regs (Base + Reg_SoH - 2), SF.SoH_SF);
      else
         Data.SOH_Percent := 100.0;
      end if;

      --  Cycle count (32-bit)
      NCyc_Hi := Regs (Base + Reg_NCyc - 2);
      NCyc_Lo := Regs (Base + Reg_NCyc - 1);
      declare
         NCyc : constant Interfaces.Unsigned_32 :=
           Interfaces.Shift_Left (Interfaces.Unsigned_32 (NCyc_Hi), 16)
           or Interfaces.Unsigned_32 (NCyc_Lo);
      begin
         Data.Cycle_Count := Natural (NCyc mod 2**31);
      end;

      --  Measurements
      Data.Voltage_V := Apply_Scale (Regs (Base + Reg_V - 2), SF.V_SF);
      Data.Current_A := Apply_Scale_Signed (Regs (Base + Reg_A - 2), SF.A_SF);
      Data.Power_W := Apply_Scale_Signed (Regs (Base + Reg_W - 2), SF.W_SF);

      --  Cell voltages
      if Is_Implemented (Regs (Base + Reg_CellVMax - 2)) then
         Data.Cell_V_Max := Apply_Scale (Regs (Base + Reg_CellVMax - 2), SF.CellV_SF);
      else
         Data.Cell_V_Max := 0.0;
      end if;

      if Is_Implemented (Regs (Base + Reg_CellVMin - 2)) then
         Data.Cell_V_Min := Apply_Scale (Regs (Base + Reg_CellVMin - 2), SF.CellV_SF);
      else
         Data.Cell_V_Min := 0.0;
      end if;

      if Is_Implemented (Regs (Base + Reg_CellVAvg - 2)) then
         Data.Cell_V_Avg := Apply_Scale (Regs (Base + Reg_CellVAvg - 2), SF.CellV_SF);
      else
         Data.Cell_V_Avg := 0.0;
      end if;

      --  Battery type
      Data.Bat_Type := (case Type_Val is
                           when 1      => Battery_Lead_Acid,
                           when 2      => Battery_NiMH,
                           when 3      => Battery_NiCd,
                           when 4      => Battery_LiIon,
                           when 5      => Battery_Carbon_Zinc,
                           when 6      => Battery_Zn_Cl,
                           when 7      => Battery_Alkaline,
                           when 8      => Battery_ReZn,
                           when 9      => Battery_NaS,
                           when 99     => Battery_Other,
                           when others => Battery_Unknown);

      --  Battery state
      Data.Bat_State := (case State_Val is
                            when 1      => State_Disconnected,
                            when 2      => State_Initializing,
                            when 3      => State_Connected,
                            when 4      => State_Standby,
                            when 5      => State_SOC_Protection,
                            when 6      => State_Suspending,
                            when 7      => State_Charging,
                            when 8      => State_Discharging,
                            when 10     => State_Fault,
                            when others => State_Unknown);

      --  Charge state
      Data.Charge_St := (case ChSt_Val is
                            when 1      => Charge_Off,
                            when 2      => Charge_Empty,
                            when 3      => Charge_Discharging,
                            when 4      => Charge_Charging,
                            when 5      => Charge_Full,
                            when 6      => Charge_Holding,
                            when 7      => Charge_Testing,
                            when others => Charge_Off);

      --  Check for alarms (Evt1 high word)
      Evt1_Hi := Regs (Base + Reg_Evt1 - 2);
      Data.Has_Alarm := Evt1_Hi /= 0;
   end Decode_Battery_Data;

end Ada_Modbus.Energy.SunSpec.Battery;
