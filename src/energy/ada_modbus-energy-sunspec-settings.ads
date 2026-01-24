--  Ada_Modbus.Energy.SunSpec.Settings - SunSpec Model 121 (Basic Settings)
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Model 121: Basic inverter configuration and operating limits.
--
--  Register offsets auto-validated against SunSpec JSON specification.

package Ada_Modbus.Energy.SunSpec.Settings
  with SPARK_Mode => On
is

   --  SunSpec Model 121: Basic Settings
   --  Total registers: 32 (header: 2, data: 30)

   Reg_WMax            : constant := 2;   --  Active Power Limit (W)
   Reg_VRef            : constant := 3;   --  Reference Voltage (V)
   Reg_VRefOfs         : constant := 4;   --  Reference Voltage Offset (V)
   Reg_VMax            : constant := 5;   --  Maximum Voltage (V)
   Reg_VMin            : constant := 6;   --  Minimum Voltage (V)
   Reg_VAMax           : constant := 7;   --  Maximum Apparent Power (VA)
   Reg_VArMaxQ1        : constant := 8;   --  Max Reactive Power Q1 (var)
   Reg_VArMaxQ2        : constant := 9;   --  Max Reactive Power Q2 (var)
   Reg_VArMaxQ3        : constant := 10;  --  Max Reactive Power Q3 (var)
   Reg_VArMaxQ4        : constant := 11;  --  Max Reactive Power Q4 (var)
   Reg_WGra            : constant := 12;  --  Power Ramp Rate (% WMax/sec)
   Reg_PFMinQ1         : constant := 13;  --  Min Power Factor Q1
   Reg_PFMinQ2         : constant := 14;  --  Min Power Factor Q2
   Reg_PFMinQ3         : constant := 15;  --  Min Power Factor Q3
   Reg_PFMinQ4         : constant := 16;  --  Min Power Factor Q4
   Reg_VArAct          : constant := 17;  --  VAr calculation method
   Reg_ClcTotVA        : constant := 18;  --  VA calculation method
   Reg_MaxRmpRte       : constant := 19;  --  Max Ramp Rate
   Reg_ECPNomHz        : constant := 20;  --  Nominal Frequency (Hz)
   Reg_ConnPh          : constant := 21;  --  Connected Phases
   Reg_WMax_SF         : constant := 22;  --  Power Scale Factor
   Reg_VRef_SF         : constant := 23;  --  Voltage Scale Factor
   Reg_VRefOfs_SF      : constant := 24;  --  Offset Scale Factor
   Reg_VMinMax_SF      : constant := 25;  --  Voltage Limits SF
   Reg_VAMax_SF        : constant := 26;  --  VA Scale Factor
   Reg_VArMax_SF       : constant := 27;  --  var Scale Factor
   Reg_WGra_SF         : constant := 28;  --  Ramp Rate SF
   Reg_PFMin_SF        : constant := 29;  --  PF Scale Factor
   Reg_MaxRmpRte_SF    : constant := 30;  --  Max Ramp SF
   Reg_ECPNomHz_SF     : constant := 31;  --  Frequency SF

   --  Connected phases enumeration
   type Connected_Phase is
     (Phase_A,
      Phase_B,
      Phase_C,
      Phase_AB,
      Phase_BC,
      Phase_CA,
      Phase_ABC);

   --  Basic settings data
   type Settings_Data is record
      Max_Power_W     : Float;   --  WMax: Active power limit
      Ref_Voltage_V   : Float;   --  VRef: Reference voltage
      Ref_Offset_V    : Float;   --  VRefOfs: Reference offset
      Max_Voltage_V   : Float;   --  VMax: Maximum voltage
      Min_Voltage_V   : Float;   --  VMin: Minimum voltage
      Max_VA          : Float;   --  VAMax: Maximum apparent power
      Ramp_Rate_Pct   : Float;   --  WGra: % WMax per second
      Nominal_Hz      : Float;   --  ECPNomHz: Nominal frequency
      Conn_Phase      : Connected_Phase;
   end record;

   --  Scale factors
   type Settings_Scale_Factors is record
      W_SF    : Scale_Factor;
      V_SF    : Scale_Factor;
      VOfs_SF : Scale_Factor;
      VLim_SF : Scale_Factor;
      VA_SF   : Scale_Factor;
      VAr_SF  : Scale_Factor;
      WGra_SF : Scale_Factor;
      PF_SF   : Scale_Factor;
      Hz_SF   : Scale_Factor;
   end record;

   --  Decode scale factors
   procedure Decode_Settings_Scale_Factors
     (Regs : Register_Array;
      SF   : out Settings_Scale_Factors)
     with Pre => Regs'Length >= 30 and then Regs'First <= Natural'Last - 32;

   --  Decode settings
   procedure Decode_Settings
     (Regs : Register_Array;
      SF   : Settings_Scale_Factors;
      Data : out Settings_Data)
     with Pre => Regs'Length >= 30 and then Regs'First <= Natural'Last - 32;

end Ada_Modbus.Energy.SunSpec.Settings;
