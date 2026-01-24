--  Integration_Test_Runner - Main runner for integration tests
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  This runner executes integration tests against a real Modbus TCP server.
--  Before running, start the simulator:
--    python modbus_simulator.py --port 5020
--
--  Usage:
--    alr build -- -P integration_tests.gpr
--    ./bin/integration_test_runner

with AUnit.Run;
with AUnit.Reporter.Text;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Text_IO; use Ada.Text_IO;

with Integration_Tests;

procedure Integration_Test_Runner is

   function Suite return Access_Test_Suite;

   function Suite return Access_Test_Suite is
      S : constant Access_Test_Suite := new Test_Suite;
   begin
      S.Add_Test (Integration_Tests.Suite);
      return S;
   end Suite;

   procedure Run is new AUnit.Run.Test_Runner (Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Put_Line ("========================================");
   Put_Line ("  AdaModbus Integration Test Suite");
   Put_Line ("========================================");
   New_Line;
   Put_Line ("This test suite requires the Modbus simulator to be running.");
   Put_Line ("Start it with:");
   Put_Line ("  python modbus_simulator.py --port 5020");
   New_Line;
   Put_Line ("Starting tests...");
   Put_Line ("----------------------------------------");
   New_Line;

   Run (Reporter);

   New_Line;
   Put_Line ("========================================");
   Put_Line ("  Tests completed");
   Put_Line ("========================================");
end Integration_Test_Runner;
