--  Integration_Tests - Modbus TCP Integration Test Suite
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Tests AdaModbus against a real Modbus TCP server (pymodbus simulator).
--  Requires the modbus_simulator.py to be running on the configured port.

with AUnit.Test_Suites;

package Integration_Tests is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Integration_Tests;
