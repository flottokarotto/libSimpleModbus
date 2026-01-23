--  Test_Runner - Main test runner for AdaModbus
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Run;
with AUnit.Reporter.Text;
with AUnit.Test_Suites; use AUnit.Test_Suites;

with Test_CRC;
with Test_LRC;
with Test_PDU;
with Test_Utilities;
with Test_RTU;
with Test_ASCII;
with Test_TCP;
with Test_Slave;
with Test_Master;
with Test_Async;
with Test_SunSpec;

procedure Test_Runner is

   function Suite return Access_Test_Suite;

   function Suite return Access_Test_Suite is
      S : constant Access_Test_Suite := new Test_Suite;
   begin
      --  Core tests
      S.Add_Test (Test_CRC.Suite);
      S.Add_Test (Test_LRC.Suite);
      S.Add_Test (Test_Utilities.Suite);

      --  Protocol tests
      S.Add_Test (Test_PDU.Suite);
      S.Add_Test (Test_RTU.Suite);
      S.Add_Test (Test_ASCII.Suite);
      S.Add_Test (Test_TCP.Suite);

      --  Integration tests with mocks
      S.Add_Test (Test_Slave.Suite);
      S.Add_Test (Test_Master.Suite);

      --  Async API tests
      S.Add_Test (Test_Async.Suite);

      --  Energy management tests
      S.Add_Test (Test_SunSpec.Suite);

      return S;
   end Suite;

   procedure Run is new AUnit.Run.Test_Runner (Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Test_Runner;
