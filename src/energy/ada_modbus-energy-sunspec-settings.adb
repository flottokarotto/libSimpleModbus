--  Ada_Modbus.Energy.SunSpec.Settings - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Energy.SunSpec.Settings
  with SPARK_Mode => On
is

   ----------------------------------
   -- Decode_Settings_Scale_Factors --
   ----------------------------------

   procedure Decode_Settings_Scale_Factors
     (Regs : Register_Array;
      SF   : out Settings_Scale_Factors)
   is
      Base : constant Natural := Regs'First;
   begin
      SF.W_SF    := To_Scale_Factor (Regs (Base + Reg_WMax_SF - 2));
      SF.V_SF    := To_Scale_Factor (Regs (Base + Reg_VRef_SF - 2));
      SF.VOfs_SF := To_Scale_Factor (Regs (Base + Reg_VRefOfs_SF - 2));
      SF.VLim_SF := To_Scale_Factor (Regs (Base + Reg_VMinMax_SF - 2));
      SF.VA_SF   := To_Scale_Factor (Regs (Base + Reg_VAMax_SF - 2));
      SF.VAr_SF  := To_Scale_Factor (Regs (Base + Reg_VArMax_SF - 2));
      SF.WGra_SF := To_Scale_Factor (Regs (Base + Reg_WGra_SF - 2));
      SF.PF_SF   := To_Scale_Factor (Regs (Base + Reg_PFMin_SF - 2));
      SF.Hz_SF   := To_Scale_Factor (Regs (Base + Reg_ECPNomHz_SF - 2));
   end Decode_Settings_Scale_Factors;

   ---------------------
   -- Decode_Settings --
   ---------------------

   procedure Decode_Settings
     (Regs : Register_Array;
      SF   : Settings_Scale_Factors;
      Data : out Settings_Data)
   is
      Base    : constant Natural := Regs'First;
      Ph_Val  : constant Natural := Natural (Regs (Base + Reg_ConnPh - 2));
   begin
      Data.Max_Power_W   := Apply_Scale (Regs (Base + Reg_WMax - 2), SF.W_SF);
      Data.Ref_Voltage_V := Apply_Scale (Regs (Base + Reg_VRef - 2), SF.V_SF);
      Data.Ref_Offset_V  := Apply_Scale_Signed (Regs (Base + Reg_VRefOfs - 2), SF.VOfs_SF);

      if Is_Implemented (Regs (Base + Reg_VMax - 2)) then
         Data.Max_Voltage_V := Apply_Scale (Regs (Base + Reg_VMax - 2), SF.VLim_SF);
      else
         Data.Max_Voltage_V := 0.0;
      end if;

      if Is_Implemented (Regs (Base + Reg_VMin - 2)) then
         Data.Min_Voltage_V := Apply_Scale (Regs (Base + Reg_VMin - 2), SF.VLim_SF);
      else
         Data.Min_Voltage_V := 0.0;
      end if;

      if Is_Implemented (Regs (Base + Reg_VAMax - 2)) then
         Data.Max_VA := Apply_Scale (Regs (Base + Reg_VAMax - 2), SF.VA_SF);
      else
         Data.Max_VA := 0.0;
      end if;

      if Is_Implemented (Regs (Base + Reg_WGra - 2)) then
         Data.Ramp_Rate_Pct := Apply_Scale (Regs (Base + Reg_WGra - 2), SF.WGra_SF);
      else
         Data.Ramp_Rate_Pct := 0.0;
      end if;

      if Is_Implemented (Regs (Base + Reg_ECPNomHz - 2)) then
         Data.Nominal_Hz := Apply_Scale (Regs (Base + Reg_ECPNomHz - 2), SF.Hz_SF);
      else
         Data.Nominal_Hz := 50.0;  --  Default EU
      end if;

      --  Connected phases
      Data.Conn_Phase := (case Ph_Val is
                             when 1 => Phase_A,
                             when 2 => Phase_B,
                             when 3 => Phase_C,
                             when 4 => Phase_AB,
                             when 5 => Phase_BC,
                             when 6 => Phase_CA,
                             when 7 => Phase_ABC,
                             when others => Phase_ABC);
   end Decode_Settings;

end Ada_Modbus.Energy.SunSpec.Settings;
