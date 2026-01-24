--  Ada_Modbus.Energy.SunSpec.Nameplate - SunSpec Model 120 (Nameplate)
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Model 120: Inverter Controls Nameplate Ratings
--  Contains rated power, voltage, current, and power factor limits.
--
--  Register offsets auto-validated against SunSpec JSON specification.

package Ada_Modbus.Energy.SunSpec.Nameplate
  with SPARK_Mode => On
is

   --  SunSpec Model 120: Nameplate
   --  Total registers: 28 (header: 2, data: 26)

   Reg_DERTyp          : constant := 2;   --  DER Type (enum16)
   Reg_WRtg            : constant := 3;   --  Max Active Power (W)
   Reg_WRtg_SF         : constant := 4;   --  Power Scale Factor
   Reg_VARtg           : constant := 5;   --  Max Apparent Power (VA)
   Reg_VARtg_SF        : constant := 6;   --  VA Scale Factor
   Reg_VArRtgQ1        : constant := 7;   --  Max Reactive Power Q1 (var)
   Reg_VArRtgQ2        : constant := 8;   --  Max Reactive Power Q2 (var)
   Reg_VArRtgQ3        : constant := 9;   --  Max Reactive Power Q3 (var)
   Reg_VArRtgQ4        : constant := 10;  --  Max Reactive Power Q4 (var)
   Reg_VArRtg_SF       : constant := 11;  --  var Scale Factor
   Reg_ARtg            : constant := 12;  --  Max Current (A)
   Reg_ARtg_SF         : constant := 13;  --  Current Scale Factor
   Reg_PFRtgQ1         : constant := 14;  --  Min Power Factor Q1
   Reg_PFRtgQ2         : constant := 15;  --  Min Power Factor Q2
   Reg_PFRtgQ3         : constant := 16;  --  Min Power Factor Q3
   Reg_PFRtgQ4         : constant := 17;  --  Min Power Factor Q4
   Reg_PFRtg_SF        : constant := 18;  --  PF Scale Factor
   Reg_WHRtg           : constant := 19;  --  Energy Rating (Wh)
   Reg_WHRtg_SF        : constant := 20;  --  Energy Scale Factor
   Reg_AhrRtg          : constant := 21;  --  Capacity Rating (Ah)
   Reg_AhrRtg_SF       : constant := 22;  --  Capacity Scale Factor
   Reg_MaxChaRte       : constant := 23;  --  Max Charge Rate (W)
   Reg_MaxChaRte_SF    : constant := 24;  --  Charge Rate Scale Factor
   Reg_MaxDisChaRte    : constant := 25;  --  Max Discharge Rate (W)
   Reg_MaxDisChaRte_SF : constant := 26;  --  Discharge Rate Scale Factor

   --  DER Type enumeration
   type DER_Type is
     (DER_PV,           --  4: Photovoltaic
      DER_PV_Storage,   --  82: PV + Storage
      DER_Storage,      --  83: Storage only
      DER_Other         --  Other types
     );

   --  Nameplate data record
   type Nameplate_Data is record
      DER_Kind        : DER_Type;
      Max_Power_W     : Float;   --  WRtg: Maximum active power
      Max_VA          : Float;   --  VARtg: Maximum apparent power
      Max_Var_Q1      : Float;   --  Reactive power limit Q1
      Max_Var_Q2      : Float;   --  Reactive power limit Q2
      Max_Var_Q3      : Float;   --  Reactive power limit Q3
      Max_Var_Q4      : Float;   --  Reactive power limit Q4
      Max_Current_A   : Float;   --  ARtg: Maximum current
      Min_PF_Q1       : Float;   --  Power factor limit Q1
      Min_PF_Q4       : Float;   --  Power factor limit Q4
      Energy_Wh       : Float;   --  WHRtg: Energy capacity (storage)
      Capacity_Ah     : Float;   --  AhrRtg: Amp-hour capacity
      Max_Charge_W    : Float;   --  Max charge rate
      Max_Discharge_W : Float;   --  Max discharge rate
   end record;

   --  Scale factors for nameplate
   type Nameplate_Scale_Factors is record
      W_SF   : Scale_Factor;
      VA_SF  : Scale_Factor;
      VAr_SF : Scale_Factor;
      A_SF   : Scale_Factor;
      PF_SF  : Scale_Factor;
      Wh_SF  : Scale_Factor;
      Ah_SF  : Scale_Factor;
      Cha_SF : Scale_Factor;
   end record;

   --  Decode nameplate scale factors
   procedure Decode_Nameplate_Scale_Factors
     (Regs : Register_Array;
      SF   : out Nameplate_Scale_Factors)
     with Pre => Regs'Length >= 27 and then Regs'First <= Natural'Last - 27;

   --  Decode nameplate data
   procedure Decode_Nameplate
     (Regs : Register_Array;
      SF   : Nameplate_Scale_Factors;
      Data : out Nameplate_Data)
     with Pre => Regs'Length >= 27 and then Regs'First <= Natural'Last - 27;

end Ada_Modbus.Energy.SunSpec.Nameplate;
