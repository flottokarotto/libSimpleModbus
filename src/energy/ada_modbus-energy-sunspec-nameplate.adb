--  Ada_Modbus.Energy.SunSpec.Nameplate - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Energy.SunSpec.Nameplate
  with SPARK_Mode => On
is

   -----------------------------------
   -- Decode_Nameplate_Scale_Factors --
   -----------------------------------

   procedure Decode_Nameplate_Scale_Factors
     (Regs : Register_Array;
      SF   : out Nameplate_Scale_Factors)
   is
      Base : constant Natural := Regs'First;
   begin
      SF.W_SF   := To_Scale_Factor (Regs (Base + Reg_WRtg_SF - 2));
      SF.VA_SF  := To_Scale_Factor (Regs (Base + Reg_VARtg_SF - 2));
      SF.VAr_SF := To_Scale_Factor (Regs (Base + Reg_VArRtg_SF - 2));
      SF.A_SF   := To_Scale_Factor (Regs (Base + Reg_ARtg_SF - 2));
      SF.PF_SF  := To_Scale_Factor (Regs (Base + Reg_PFRtg_SF - 2));
      SF.Wh_SF  := To_Scale_Factor (Regs (Base + Reg_WHRtg_SF - 2));
      SF.Ah_SF  := To_Scale_Factor (Regs (Base + Reg_AhrRtg_SF - 2));
      SF.Cha_SF := To_Scale_Factor (Regs (Base + Reg_MaxChaRte_SF - 2));
   end Decode_Nameplate_Scale_Factors;

   ----------------------
   -- Decode_Nameplate --
   ----------------------

   procedure Decode_Nameplate
     (Regs : Register_Array;
      SF   : Nameplate_Scale_Factors;
      Data : out Nameplate_Data)
   is
      Base : constant Natural := Regs'First;
      DER_Val : constant Natural := Natural (Regs (Base + Reg_DERTyp - 2));
   begin
      --  DER Type
      Data.DER_Kind := (case DER_Val is
                           when 4      => DER_PV,
                           when 82     => DER_PV_Storage,
                           when 83     => DER_Storage,
                           when others => DER_Other);

      --  Power ratings
      Data.Max_Power_W := Apply_Scale (Regs (Base + Reg_WRtg - 2), SF.W_SF);
      Data.Max_VA      := Apply_Scale (Regs (Base + Reg_VARtg - 2), SF.VA_SF);

      --  Reactive power limits (signed)
      Data.Max_Var_Q1 := Apply_Scale_Signed (Regs (Base + Reg_VArRtgQ1 - 2), SF.VAr_SF);
      Data.Max_Var_Q2 := Apply_Scale_Signed (Regs (Base + Reg_VArRtgQ2 - 2), SF.VAr_SF);
      Data.Max_Var_Q3 := Apply_Scale_Signed (Regs (Base + Reg_VArRtgQ3 - 2), SF.VAr_SF);
      Data.Max_Var_Q4 := Apply_Scale_Signed (Regs (Base + Reg_VArRtgQ4 - 2), SF.VAr_SF);

      --  Current and PF limits
      Data.Max_Current_A := Apply_Scale (Regs (Base + Reg_ARtg - 2), SF.A_SF);
      Data.Min_PF_Q1     := Apply_Scale_Signed (Regs (Base + Reg_PFRtgQ1 - 2), SF.PF_SF);
      Data.Min_PF_Q4     := Apply_Scale_Signed (Regs (Base + Reg_PFRtgQ4 - 2), SF.PF_SF);

      --  Storage-related (may be 0 if not applicable)
      if Is_Implemented (Regs (Base + Reg_WHRtg - 2)) then
         Data.Energy_Wh := Apply_Scale (Regs (Base + Reg_WHRtg - 2), SF.Wh_SF);
      else
         Data.Energy_Wh := 0.0;
      end if;

      if Is_Implemented (Regs (Base + Reg_AhrRtg - 2)) then
         Data.Capacity_Ah := Apply_Scale (Regs (Base + Reg_AhrRtg - 2), SF.Ah_SF);
      else
         Data.Capacity_Ah := 0.0;
      end if;

      if Is_Implemented (Regs (Base + Reg_MaxChaRte - 2)) then
         Data.Max_Charge_W := Apply_Scale (Regs (Base + Reg_MaxChaRte - 2), SF.Cha_SF);
      else
         Data.Max_Charge_W := 0.0;
      end if;

      if Is_Implemented (Regs (Base + Reg_MaxDisChaRte - 2)) then
         Data.Max_Discharge_W := Apply_Scale (Regs (Base + Reg_MaxDisChaRte - 2), SF.Cha_SF);
      else
         Data.Max_Discharge_W := 0.0;
      end if;
   end Decode_Nameplate;

end Ada_Modbus.Energy.SunSpec.Nameplate;
