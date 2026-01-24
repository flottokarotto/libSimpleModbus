--  Ada_Modbus.Energy.SunSpec.Battery - SunSpec Model 802 (Battery Base)
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Model 802: Battery Base Model
--  Extended battery monitoring beyond Model 124 (Basic Storage):
--  - Cell-level voltage monitoring
--  - Battery state and health
--  - Charge/discharge control
--  - Event flags and alarms
--
--  Register offsets auto-validated against SunSpec JSON specification.

package Ada_Modbus.Energy.SunSpec.Battery
  with SPARK_Mode => On
is

   --  Model ID
   Model_Battery_Base : constant := 802;

   --  SunSpec Model 802: Battery Base Model
   --  Total registers: 64 (header: 2, data: 62)

   --  Ratings and limits
   Reg_AHRtg           : constant := 2;   --  Amp-hour rating (Ah)
   Reg_WHRtg           : constant := 3;   --  Energy rating (Wh)
   Reg_WChaRteMax      : constant := 4;   --  Max charge rate (W)
   Reg_WDisChaRteMax   : constant := 5;   --  Max discharge rate (W)
   Reg_DisChaRte       : constant := 6;   --  Discharge rate (% WHRtg)
   Reg_SoCMax          : constant := 7;   --  Max SOC (%)
   Reg_SoCMin          : constant := 8;   --  Min SOC (%)
   Reg_SocRsvMax       : constant := 9;   --  Max SOC reserve (%)
   Reg_SoCRsvMin       : constant := 10;  --  Min SOC reserve (%)

   --  Current state
   Reg_SoC             : constant := 11;  --  State of Charge (%)
   Reg_DoD             : constant := 12;  --  Depth of Discharge (%)
   Reg_SoH             : constant := 13;  --  State of Health (%)
   Reg_NCyc            : constant := 14;  --  Cycle count (uint32)
   Reg_ChaSt           : constant := 16;  --  Charge state (enum)
   Reg_LocRemCtl       : constant := 17;  --  Local/Remote control
   Reg_Hb              : constant := 18;  --  Heartbeat
   Reg_CtrlHb          : constant := 19;  --  Control heartbeat
   Reg_AlmRst          : constant := 20;  --  Alarm reset

   --  Battery type and state
   Reg_Typ             : constant := 21;  --  Battery type
   Reg_State           : constant := 22;  --  Battery state
   Reg_StateVnd        : constant := 23;  --  Vendor state
   Reg_WarrDt          : constant := 24;  --  Warranty date (uint32)

   --  Events
   Reg_Evt1            : constant := 26;  --  Event flags 1 (bitfield32)
   Reg_Evt2            : constant := 28;  --  Event flags 2 (bitfield32)
   Reg_EvtVnd1         : constant := 30;  --  Vendor events 1 (bitfield32)
   Reg_EvtVnd2         : constant := 32;  --  Vendor events 2 (bitfield32)

   --  Voltage and current
   Reg_V               : constant := 34;  --  Battery voltage (V)
   Reg_VMax            : constant := 35;  --  Max voltage (V)
   Reg_VMin            : constant := 36;  --  Min voltage (V)
   Reg_CellVMax        : constant := 37;  --  Max cell voltage (V)
   Reg_CellVMaxStr     : constant := 38;  --  String with max cell V
   Reg_CellVMaxMod     : constant := 39;  --  Module with max cell V
   Reg_CellVMin        : constant := 40;  --  Min cell voltage (V)
   Reg_CellVMinStr     : constant := 41;  --  String with min cell V
   Reg_CellVMinMod     : constant := 42;  --  Module with min cell V
   Reg_CellVAvg        : constant := 43;  --  Average cell voltage (V)
   Reg_A               : constant := 44;  --  Battery current (A)
   Reg_AChaMax         : constant := 45;  --  Max charge current (A)
   Reg_ADisChaMax      : constant := 46;  --  Max discharge current (A)
   Reg_W               : constant := 47;  --  Battery power (W)

   --  Control
   Reg_ReqInvState     : constant := 48;  --  Requested inverter state
   Reg_ReqW            : constant := 49;  --  Requested power (W)
   Reg_SetOp           : constant := 50;  --  Set operation
   Reg_SetInvState     : constant := 51;  --  Set inverter state

   --  Scale factors
   Reg_AHRtg_SF        : constant := 52;
   Reg_WHRtg_SF        : constant := 53;
   Reg_WChaDisChaMax_SF : constant := 54;
   Reg_DisChaRte_SF    : constant := 55;
   Reg_SoC_SF          : constant := 56;
   Reg_DoD_SF          : constant := 57;
   Reg_SoH_SF          : constant := 58;
   Reg_V_SF            : constant := 59;
   Reg_CellV_SF        : constant := 60;
   Reg_A_SF            : constant := 61;
   Reg_AMax_SF         : constant := 62;
   Reg_W_SF            : constant := 63;

   --  Battery types
   type Battery_Type is
     (Battery_Unknown,
      Battery_Lead_Acid,
      Battery_NiMH,
      Battery_NiCd,
      Battery_LiIon,
      Battery_Carbon_Zinc,
      Battery_Zn_Cl,
      Battery_Alkaline,
      Battery_ReZn,
      Battery_NaS,
      Battery_Other);

   --  Battery states
   type Battery_State is
     (State_Disconnected,
      State_Initializing,
      State_Connected,
      State_Standby,
      State_SOC_Protection,
      State_Suspending,
      State_Charging,
      State_Discharging,
      State_Fault,
      State_Unknown);

   --  Charge states
   type Charge_State is
     (Charge_Off,
      Charge_Empty,
      Charge_Discharging,
      Charge_Charging,
      Charge_Full,
      Charge_Holding,
      Charge_Testing);

   --  Battery data record
   type Battery_Data is record
      --  Ratings
      Capacity_Ah       : Float;   --  Amp-hour capacity
      Capacity_Wh       : Float;   --  Energy capacity
      Max_Charge_W      : Float;   --  Max charge power
      Max_Discharge_W   : Float;   --  Max discharge power

      --  State
      SOC_Percent       : Float;   --  State of charge
      DOD_Percent       : Float;   --  Depth of discharge
      SOH_Percent       : Float;   --  State of health
      Cycle_Count       : Natural; --  Number of cycles

      --  Measurements
      Voltage_V         : Float;   --  Battery voltage
      Current_A         : Float;   --  Current (+ charge, - discharge)
      Power_W           : Float;   --  Power (+ charge, - discharge)
      Cell_V_Max        : Float;   --  Highest cell voltage
      Cell_V_Min        : Float;   --  Lowest cell voltage
      Cell_V_Avg        : Float;   --  Average cell voltage

      --  Status
      Bat_Type          : Battery_Type;
      Bat_State         : Battery_State;
      Charge_St         : Charge_State;
      Has_Alarm         : Boolean;
   end record;

   --  Scale factors
   type Battery_Scale_Factors is record
      Ah_SF    : Scale_Factor;
      Wh_SF    : Scale_Factor;
      WRate_SF : Scale_Factor;
      SoC_SF   : Scale_Factor;
      DoD_SF   : Scale_Factor;
      SoH_SF   : Scale_Factor;
      V_SF     : Scale_Factor;
      CellV_SF : Scale_Factor;
      A_SF     : Scale_Factor;
      W_SF     : Scale_Factor;
   end record;

   --  Decode scale factors
   procedure Decode_Battery_Scale_Factors
     (Regs : Register_Array;
      SF   : out Battery_Scale_Factors)
     with Pre => Regs'Length >= 62 and then Regs'First <= Natural'Last - 65;

   --  Decode battery data
   procedure Decode_Battery_Data
     (Regs : Register_Array;
      SF   : Battery_Scale_Factors;
      Data : out Battery_Data)
     with Pre => Regs'Length >= 62 and then Regs'First <= Natural'Last - 65;

end Ada_Modbus.Energy.SunSpec.Battery;
