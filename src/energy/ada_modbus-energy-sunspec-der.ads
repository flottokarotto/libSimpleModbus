--  Ada_Modbus.Energy.SunSpec.DER - DER (Distributed Energy Resource) Controls
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  SunSpec DER Models for grid integration and control:
--  - Model 701: DER AC Measurements (status, power, energy)
--  - Model 704: DER AC Controls (power limits, setpoints)
--
--  These models are essential for:
--  - §14a EnWG compliance (German grid control)
--  - Smart grid integration
--  - Power curtailment and reactive power control
--
--  Register offsets auto-validated against SunSpec JSON specification.

package Ada_Modbus.Energy.SunSpec.DER
  with SPARK_Mode => On
is

   --  Model IDs
   Model_DER_AC_Measurement : constant := 701;
   Model_DER_AC_Controls    : constant := 704;

   ---------------------------------------------------------------------------
   --  Model 704: DER AC Controls
   --  Total registers: 59 (header: 2, data: 57)
   --  Used for power limiting and reactive power control
   ---------------------------------------------------------------------------

   --  Power limiting registers
   Reg_WMaxLimPctEna      : constant := 14;  --  Enable power limit (enum16)
   Reg_WMaxLimPct         : constant := 15;  --  Power limit (% of WMax)
   Reg_WMaxLimPctRvrt     : constant := 16;  --  Revert power limit (%)
   Reg_WMaxLimPctEnaRvrt  : constant := 17;  --  Enable revert
   Reg_WMaxLimPctRvrtTms  : constant := 18;  --  Revert timeout (uint32, secs)
   Reg_WMaxLimPctRvrtRem  : constant := 20;  --  Remaining revert time

   --  Active power setpoint
   Reg_WSetEna            : constant := 22;  --  Enable W setpoint
   Reg_WSetMod            : constant := 23;  --  Setpoint mode
   Reg_WSet               : constant := 24;  --  Power setpoint (int32, W)
   Reg_WSetRvrt           : constant := 26;  --  Revert setpoint (int32, W)
   Reg_WSetPct            : constant := 28;  --  Setpoint % of WMax
   Reg_WSetPctRvrt        : constant := 29;  --  Revert setpoint %

   --  Reactive power setpoint
   Reg_VarSetEna          : constant := 35;  --  Enable var setpoint
   Reg_VarSetMod          : constant := 36;  --  Setpoint mode
   Reg_VarSetPri          : constant := 37;  --  Priority
   Reg_VarSet             : constant := 38;  --  var setpoint (int32)
   Reg_VarSetRvrt         : constant := 40;  --  Revert setpoint
   Reg_VarSetPct          : constant := 42;  --  Setpoint % of VArMax
   Reg_VarSetPctRvrt      : constant := 43;  --  Revert setpoint %

   --  Ramp rates
   Reg_WRmp               : constant := 49;  --  W ramp rate (%Max/sec)
   Reg_WRmpRef            : constant := 50;  --  Ramp reference
   Reg_VarRmp             : constant := 51;  --  var ramp rate

   --  Anti-islanding
   Reg_AntiIslEna         : constant := 52;  --  Anti-islanding enable

   --  Scale factors
   Reg_704_WMaxLimPct_SF  : constant := 54;  --  Power limit SF
   Reg_704_WSet_SF        : constant := 55;  --  W setpoint SF
   Reg_704_WSetPct_SF     : constant := 56;  --  W setpoint % SF
   Reg_704_VarSet_SF      : constant := 57;  --  var setpoint SF
   Reg_704_VarSetPct_SF   : constant := 58;  --  var setpoint % SF

   ---------------------------------------------------------------------------
   --  Model 701: DER AC Measurements (key registers only)
   --  Total registers: 155 (header: 2, data: 153)
   ---------------------------------------------------------------------------

   Reg_701_ACType         : constant := 2;   --  AC wiring type
   Reg_701_St             : constant := 3;   --  Operating state
   Reg_701_InvSt          : constant := 4;   --  Inverter state
   Reg_701_ConnSt         : constant := 5;   --  Grid connection state
   Reg_701_Alrm           : constant := 6;   --  Alarms (bitfield32)
   Reg_701_DERMode        : constant := 8;   --  DER mode (bitfield32)
   Reg_701_W              : constant := 10;  --  Active power (W)
   Reg_701_VA             : constant := 11;  --  Apparent power (VA)
   Reg_701_Var            : constant := 12;  --  Reactive power (var)
   Reg_701_PF             : constant := 13;  --  Power factor
   Reg_701_A              : constant := 14;  --  Current (A)
   Reg_701_Hz             : constant := 17;  --  Frequency (uint32, Hz)
   Reg_701_ThrotPct       : constant := 110; --  Throttle percentage
   Reg_701_ThrotSrc       : constant := 111; --  Throttle source

   --  Scale factors for Model 701
   Reg_701_A_SF           : constant := 113;
   Reg_701_V_SF           : constant := 114;
   Reg_701_Hz_SF          : constant := 115;
   Reg_701_W_SF           : constant := 116;
   Reg_701_PF_SF          : constant := 117;
   Reg_701_VA_SF          : constant := 118;
   Reg_701_Var_SF         : constant := 119;

   --  Enable/Disable enumeration
   type Enable_Mode is (Disabled, Enabled);

   --  Operating states
   type DER_State is
     (State_Off,
      State_Sleeping,
      State_Starting,
      State_Running,
      State_Throttled,
      State_Shutting_Down,
      State_Fault,
      State_Standby,
      State_Unknown);

   --  Connection states
   type Connection_State is
     (Disconnected,
      Connected,
      Unknown_Conn);

   --  DER Control settings (for writing)
   type DER_Control_Settings is record
      --  Power limiting
      Power_Limit_Enabled   : Enable_Mode;
      Power_Limit_Pct       : Float;         --  0-100%
      Power_Limit_Revert_S  : Natural;       --  Revert timeout (0 = no revert)

      --  Reactive power
      Var_Setpoint_Enabled  : Enable_Mode;
      Var_Setpoint_Pct      : Float;         --  % of VArMax

      --  Ramp rates
      Power_Ramp_Pct_Sec    : Float;         --  % per second
   end record;

   --  DER Status (from Model 701)
   type DER_Status is record
      Operating_State    : DER_State;
      Conn_State         : Connection_State;
      Active_Power_W     : Float;
      Reactive_Power_Var : Float;
      Power_Factor       : Float;
      Frequency_Hz       : Float;
      Throttle_Pct       : Float;
      Is_Throttled       : Boolean;
   end record;

   --  Scale factors for Model 704
   type DER_Control_Scale_Factors is record
      WLimPct_SF  : Scale_Factor;
      WSet_SF     : Scale_Factor;
      WSetPct_SF  : Scale_Factor;
      VarSet_SF   : Scale_Factor;
      VarSetPct_SF : Scale_Factor;
   end record;

   --  Scale factors for Model 701
   type DER_Status_Scale_Factors is record
      W_SF   : Scale_Factor;
      VA_SF  : Scale_Factor;
      Var_SF : Scale_Factor;
      PF_SF  : Scale_Factor;
      Hz_SF  : Scale_Factor;
   end record;

   --  Decode Model 704 scale factors
   procedure Decode_Control_Scale_Factors
     (Regs : Register_Array;
      SF   : out DER_Control_Scale_Factors)
     with Pre => Regs'Length >= 57 and then Regs'First <= Natural'Last - 60;

   --  Decode DER control settings from Model 704
   procedure Decode_Control_Settings
     (Regs     : Register_Array;
      SF       : DER_Control_Scale_Factors;
      Settings : out DER_Control_Settings)
     with Pre => Regs'Length >= 57 and then Regs'First <= Natural'Last - 60;

   --  Decode Model 701 scale factors
   procedure Decode_Status_Scale_Factors
     (Regs : Register_Array;
      SF   : out DER_Status_Scale_Factors)
     with Pre => Regs'Length >= 120 and then Regs'First <= Natural'Last - 120;

   --  Decode DER status from Model 701
   procedure Decode_DER_Status
     (Regs   : Register_Array;
      SF     : DER_Status_Scale_Factors;
      Status : out DER_Status)
     with Pre => Regs'Length >= 120 and then Regs'First <= Natural'Last - 120;

   --  Encode power limit command (returns registers to write at offset 14)
   procedure Encode_Power_Limit
     (Limit_Pct : Float;
      Enabled   : Boolean;
      Regs      : out Register_Array)
     with Pre => Regs'Length = 4;

end Ada_Modbus.Energy.SunSpec.DER;
