--  Ada_Modbus.Energy.SunSpec.Meter - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Energy.SunSpec.Meter
  with SPARK_Mode => On
is

   --------------------------------
   -- Decode_Meter_Scale_Factors --
   --------------------------------

   procedure Decode_Meter_Scale_Factors
     (Regs : Register_Array;
      SF   : out Meter_Scale_Factors)
   is
      Base : constant Natural := Regs'First;
   begin
      SF.A_SF   := To_Scale_Factor (Regs (Base + Reg_A_SF - 2));
      SF.V_SF   := To_Scale_Factor (Regs (Base + Reg_V_SF - 2));
      SF.Hz_SF  := To_Scale_Factor (Regs (Base + Reg_Hz_SF - 2));
      SF.W_SF   := To_Scale_Factor (Regs (Base + Reg_W_SF - 2));
      SF.VA_SF  := To_Scale_Factor (Regs (Base + Reg_VA_SF - 2));
      SF.VAR_SF := To_Scale_Factor (Regs (Base + Reg_VAR_SF - 2));
      SF.PF_SF  := To_Scale_Factor (Regs (Base + Reg_PF_SF - 2));
      SF.Wh_SF  := To_Scale_Factor (Regs (Base + Reg_TotWh_SF - 2));
   end Decode_Meter_Scale_Factors;

   --------------------------
   -- Decode_Meter_Totals --
   --------------------------

   procedure Decode_Meter_Totals
     (Regs       : Register_Array;
      SF         : Meter_Scale_Factors;
      Meter_Kind : Meter_Type;
      Data       : out Meter_Data)
   is
      Base : constant Natural := Regs'First;

      --  Helper to apply scale factor with signed interpretation
      function Scaled_Signed (Reg_Offset : Natural; S : Scale_Factor) return Float is
         Val : constant Register_Value := Regs (Base + Reg_Offset - 2);
      begin
         if Is_Implemented_Int16 (Val) then
            return Apply_Scale_Signed (Val, S);
         else
            return 0.0;
         end if;
      end Scaled_Signed;

      --  Helper for unsigned values
      function Scaled (Reg_Offset : Natural; S : Scale_Factor) return Float is
         Val : constant Register_Value := Regs (Base + Reg_Offset - 2);
      begin
         if Is_Implemented (Val) then
            return Apply_Scale (Val, S);
         else
            return 0.0;
         end if;
      end Scaled;

   begin
      Data.Meter_Kind := Meter_Kind;

      --  Totals
      Data.Total_Current    := Scaled_Signed (Reg_A, SF.A_SF);
      Data.Total_Voltage    := Scaled (Reg_PhV, SF.V_SF);   --  L-N average
      Data.Total_Voltage_LL := Scaled (Reg_PPV, SF.V_SF);   --  L-L average
      Data.Total_Power      := Scaled_Signed (Reg_W, SF.W_SF);
      Data.Total_VA      := Scaled_Signed (Reg_VA, SF.VA_SF);
      Data.Total_VAR     := Scaled_Signed (Reg_VAR, SF.VAR_SF);
      Data.Total_PF      := Scaled_Signed (Reg_PF, SF.PF_SF);
      Data.Frequency     := Scaled (Reg_Hz, SF.Hz_SF);

      --  Initialize energy to 0 (decoded in Decode_Meter_Phases if available)
      Data.Total_Exp_Wh := 0.0;
      Data.Total_Imp_Wh := 0.0;

      --  Initialize phases
      Data.Phase_A := (others => 0.0);
      Data.Phase_B := (others => 0.0);
      Data.Phase_C := (others => 0.0);
   end Decode_Meter_Totals;

   --------------------------
   -- Decode_Meter_Phases --
   --------------------------

   procedure Decode_Meter_Phases
     (Regs : Register_Array;
      SF   : Meter_Scale_Factors;
      Data : in out Meter_Data)
   is
      Base : constant Natural := Regs'First;

      --  Helper for signed values
      function Scaled_Signed (Reg_Offset : Natural; S : Scale_Factor) return Float is
         Val : constant Register_Value := Regs (Base + Reg_Offset - 2);
      begin
         if Is_Implemented_Int16 (Val) then
            return Apply_Scale_Signed (Val, S);
         else
            return 0.0;
         end if;
      end Scaled_Signed;

      --  Helper for unsigned values
      function Scaled (Reg_Offset : Natural; S : Scale_Factor) return Float is
         Val : constant Register_Value := Regs (Base + Reg_Offset - 2);
      begin
         if Is_Implemented (Val) then
            return Apply_Scale (Val, S);
         else
            return 0.0;
         end if;
      end Scaled;

      --  Decode 32-bit energy value
      function Decode_Energy (Reg_Offset : Natural) return Float is
         use type Interfaces.Unsigned_32;
         Hi  : constant Register_Value := Regs (Base + Reg_Offset - 2);
         Lo  : constant Register_Value := Regs (Base + Reg_Offset - 1);
         Val : Interfaces.Unsigned_32;
         Result : Float;
      begin
         if Hi = Not_Implemented and then Lo = Not_Implemented then
            return 0.0;
         end if;
         Val := Interfaces.Shift_Left (Interfaces.Unsigned_32 (Hi), 16) or
                Interfaces.Unsigned_32 (Lo);
         --  Val is bounded to uint32, Scale_Multipliers to 1E-10..1E10
         --  Maximum result: 4.29E9 * 1E10 = 4.29E19 < Float'Last (3.4E38)
         Result := Float (Val) * Scale_Multipliers (SF.Wh_SF);
         pragma Annotate (GNATprove, Intentional,
                          "float overflow check might fail",
                          "Val bounded to uint32 range, result < Float'Last");
         return Result;
      end Decode_Energy;

   begin
      --  Phase A
      Data.Phase_A.Current_A  := Scaled_Signed (Reg_AphA, SF.A_SF);
      Data.Phase_A.Voltage_V  := Scaled (Reg_PhVphA, SF.V_SF);
      Data.Phase_A.Power_W    := Scaled_Signed (Reg_WphA, SF.W_SF);
      Data.Phase_A.VA         := Scaled_Signed (Reg_VAphA, SF.VA_SF);
      Data.Phase_A.VAR        := Scaled_Signed (Reg_VARphA, SF.VAR_SF);
      Data.Phase_A.PF         := Scaled_Signed (Reg_PFphA, SF.PF_SF);
      Data.Phase_A.Energy_Exp := Decode_Energy (Reg_TotWhExpPhA);
      Data.Phase_A.Energy_Imp := Decode_Energy (Reg_TotWhImpPhA);

      --  Phase B
      Data.Phase_B.Current_A  := Scaled_Signed (Reg_AphB, SF.A_SF);
      Data.Phase_B.Voltage_V  := Scaled (Reg_PhVphB, SF.V_SF);
      Data.Phase_B.Power_W    := Scaled_Signed (Reg_WphB, SF.W_SF);
      Data.Phase_B.VA         := Scaled_Signed (Reg_VAphB, SF.VA_SF);
      Data.Phase_B.VAR        := Scaled_Signed (Reg_VARphB, SF.VAR_SF);
      Data.Phase_B.PF         := Scaled_Signed (Reg_PFphB, SF.PF_SF);
      Data.Phase_B.Energy_Exp := Decode_Energy (Reg_TotWhExpPhB);
      Data.Phase_B.Energy_Imp := Decode_Energy (Reg_TotWhImpPhB);

      --  Phase C
      Data.Phase_C.Current_A  := Scaled_Signed (Reg_AphC, SF.A_SF);
      Data.Phase_C.Voltage_V  := Scaled (Reg_PhVphC, SF.V_SF);
      Data.Phase_C.Power_W    := Scaled_Signed (Reg_WphC, SF.W_SF);
      Data.Phase_C.VA         := Scaled_Signed (Reg_VAphC, SF.VA_SF);
      Data.Phase_C.VAR        := Scaled_Signed (Reg_VARphC, SF.VAR_SF);
      Data.Phase_C.PF         := Scaled_Signed (Reg_PFphC, SF.PF_SF);
      Data.Phase_C.Energy_Exp := Decode_Energy (Reg_TotWhExpPhC);
      Data.Phase_C.Energy_Imp := Decode_Energy (Reg_TotWhImpPhC);

      --  Total energy
      Data.Total_Exp_Wh := Decode_Energy (Reg_TotWhExp);
      Data.Total_Imp_Wh := Decode_Energy (Reg_TotWhImp);
   end Decode_Meter_Phases;

   --------------------
   -- To_Meter_Type --
   --------------------

   function To_Meter_Type (ID : Model_ID) return Meter_Type is
   begin
      case ID is
         when 201    => return Meter_1P;
         when 202    => return Meter_SP;
         when 203    => return Meter_3P_Wye;
         when 204    => return Meter_3P_Delta;
         when others => return Meter_1P;  --  Default fallback
      end case;
   end To_Meter_Type;

end Ada_Modbus.Energy.SunSpec.Meter;
