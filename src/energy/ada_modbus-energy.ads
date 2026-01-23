--  Ada_Modbus.Energy - Energy Management Extensions
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides high-level abstractions for common energy management use cases:
--  - SG_Ready: Heat pump control (2/4-state switching)
--  - Grid_Control: §14a power limitation for grid operators

package Ada_Modbus.Energy
  with Pure
is

   --  Power limitation as percentage (0 = off, 100 = full power)
   type Power_Percent is range 0 .. 100;

   --  Common device states
   type Device_State is (Off, Standby, Running, Error);

end Ada_Modbus.Energy;
