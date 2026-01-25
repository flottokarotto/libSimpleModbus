--  Ada_Modbus.Energy.SunSpec.Meter - SunSpec Meter Models (201-204)
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Meter models for energy metering:
--  - Model 201: Single Phase Meter
--  - Model 202: Split Phase Meter
--  - Model 203: Wye-Connect Three Phase Meter
--  - Model 204: Delta-Connect Three Phase Meter
--
--  These models provide bidirectional energy measurements.

package Ada_Modbus.Energy.SunSpec.Meter
  with SPARK_Mode => On
is

   --  Meter model register offsets (after 2-reg header)
   --  Model 203 (Wye 3-Phase) layout:
   Reg_A            : constant := 2;   --  Total Current (int16)
   Reg_AphA         : constant := 3;   --  Phase A Current
   Reg_AphB         : constant := 4;   --  Phase B Current
   Reg_AphC         : constant := 5;   --  Phase C Current
   Reg_A_SF         : constant := 6;   --  Current Scale Factor

   Reg_PhV          : constant := 7;   --  Voltage L-N average
   Reg_PhVphA       : constant := 8;   --  Phase A Voltage L-N
   Reg_PhVphB       : constant := 9;   --  Phase B Voltage L-N
   Reg_PhVphC       : constant := 10;  --  Phase C Voltage L-N
   Reg_PPV          : constant := 11;  --  Voltage L-L average
   Reg_PPVphAB      : constant := 12;  --  Voltage L-L AB
   Reg_PPVphBC      : constant := 13;  --  Voltage L-L BC
   Reg_PPVphCA      : constant := 14;  --  Voltage L-L CA
   Reg_V_SF         : constant := 15;  --  Voltage Scale Factor

   Reg_Hz           : constant := 16;  --  Frequency
   Reg_Hz_SF        : constant := 17;  --  Frequency Scale Factor

   Reg_W            : constant := 18;  --  Total Active Power (int16)
   Reg_WphA         : constant := 19;  --  Phase A Active Power
   Reg_WphB         : constant := 20;  --  Phase B Active Power
   Reg_WphC         : constant := 21;  --  Phase C Active Power
   Reg_W_SF         : constant := 22;  --  Power Scale Factor

   Reg_VA           : constant := 23;  --  Total Apparent Power
   Reg_VAphA        : constant := 24;  --  Phase A Apparent Power
   Reg_VAphB        : constant := 25;  --  Phase B Apparent Power
   Reg_VAphC        : constant := 26;  --  Phase C Apparent Power
   Reg_VA_SF        : constant := 27;  --  VA Scale Factor

   Reg_VAR          : constant := 28;  --  Total Reactive Power
   Reg_VARphA       : constant := 29;  --  Phase A Reactive Power
   Reg_VARphB       : constant := 30;  --  Phase B Reactive Power
   Reg_VARphC       : constant := 31;  --  Phase C Reactive Power
   Reg_VAR_SF       : constant := 32;  --  VAR Scale Factor

   Reg_PF           : constant := 33;  --  Total Power Factor
   Reg_PFphA        : constant := 34;  --  Phase A Power Factor
   Reg_PFphB        : constant := 35;  --  Phase B Power Factor
   Reg_PFphC        : constant := 36;  --  Phase C Power Factor
   Reg_PF_SF        : constant := 37;  --  PF Scale Factor

   Reg_TotWhExp     : constant := 38;  --  Total Wh Exported (uint32)
   Reg_TotWhExpPhA  : constant := 40;  --  Phase A Wh Exported
   Reg_TotWhExpPhB  : constant := 42;  --  Phase B Wh Exported
   Reg_TotWhExpPhC  : constant := 44;  --  Phase C Wh Exported

   Reg_TotWhImp     : constant := 46;  --  Total Wh Imported (uint32)
   Reg_TotWhImpPhA  : constant := 48;  --  Phase A Wh Imported
   Reg_TotWhImpPhB  : constant := 50;  --  Phase B Wh Imported
   Reg_TotWhImpPhC  : constant := 52;  --  Phase C Wh Imported

   Reg_TotWh_SF     : constant := 54;  --  Energy Scale Factor

   --  Meter types
   type Meter_Type is
     (Meter_1P,      --  Model 201: Single Phase
      Meter_SP,      --  Model 202: Split Phase
      Meter_3P_Wye,  --  Model 203: Three Phase Wye
      Meter_3P_Delta --  Model 204: Three Phase Delta
     );

   --  Per-phase measurements
   type Phase_Data is record
      Current_A   : Float;
      Voltage_V   : Float;
      Power_W     : Float;
      VA          : Float;
      VAR         : Float;
      PF          : Float;
      Energy_Exp  : Float;  --  Wh exported
      Energy_Imp  : Float;  --  Wh imported
   end record;

   --  Complete meter data
   type Meter_Data is record
      Meter_Kind      : Meter_Type;
      --  Totals
      Total_Current   : Float;  --  A
      Total_Voltage   : Float;  --  V (L-N avg)
      Total_Voltage_LL : Float; --  V (L-L avg, ~400V in Europe)
      Total_Power     : Float;  --  W (signed, negative = export)
      Total_VA        : Float;  --  Apparent power
      Total_VAR       : Float;  --  Reactive power
      Total_PF        : Float;  --  Power factor
      Frequency       : Float;  --  Hz
      Total_Exp_Wh    : Float;  --  Total exported energy
      Total_Imp_Wh    : Float;  --  Total imported energy
      --  Per-phase (valid for 3-phase models)
      Phase_A         : Phase_Data;
      Phase_B         : Phase_Data;
      Phase_C         : Phase_Data;
   end record;

   --  Scale factors extracted from meter registers
   type Meter_Scale_Factors is record
      A_SF   : Scale_Factor;  --  Current
      V_SF   : Scale_Factor;  --  Voltage
      Hz_SF  : Scale_Factor;  --  Frequency
      W_SF   : Scale_Factor;  --  Power
      VA_SF  : Scale_Factor;  --  Apparent
      VAR_SF : Scale_Factor;  --  Reactive
      PF_SF  : Scale_Factor;  --  Power Factor
      Wh_SF  : Scale_Factor;  --  Energy
   end record;

   --  Decode meter scale factors from registers
   --  Input: Register array containing at least up to Reg_TotWh_SF
   procedure Decode_Meter_Scale_Factors
     (Regs : Register_Array;
      SF   : out Meter_Scale_Factors)
     with Pre => Regs'Length >= 55;

   --  Decode basic meter measurements (totals only)
   --  Input: Register array with at least 38 registers
   procedure Decode_Meter_Totals
     (Regs       : Register_Array;
      SF         : Meter_Scale_Factors;
      Meter_Kind : Meter_Type;
      Data       : out Meter_Data)
     with Pre => Regs'Length >= 38;

   --  Decode per-phase measurements
   --  Input: Full register array (55+ registers)
   procedure Decode_Meter_Phases
     (Regs : Register_Array;
      SF   : Meter_Scale_Factors;
      Data : in out Meter_Data)
     with Pre => Regs'Length >= 55;

   --  Convert Model ID to Meter Type
   function To_Meter_Type (ID : Model_ID) return Meter_Type;

end Ada_Modbus.Energy.SunSpec.Meter;
