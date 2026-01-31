--  Test_Async - Async Master API tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.RTU;
with Ada_Modbus.Master;
with Ada_Modbus.Master.Async;

package body Test_Async is

   --  Mock transport state
   Async_Send_Buffer   : Byte_Array (0 .. 511) := [others => 0];
   pragma Unreferenced (Async_Send_Buffer);  --  Used only for storage
   Async_Send_Length   : Natural := 0;
   Async_Recv_Buffer   : Byte_Array (0 .. 511) := [others => 0];
   Async_Recv_Length   : Natural := 0;
   Async_Recv_Pos      : Natural := 0;
   Async_Time          : Unsigned_32 := 0;
   Async_Send_Fail     : Boolean := False;
   Async_Recv_Available: Boolean := False;

   --  Callback tracking
   Callback_Called     : Boolean := False;
   Callback_Status     : Status := Success;
   Callback_Values     : Register_Array (0 .. 9) := [others => 0];
   Callback_Value_Count: Natural := 0;

   type Mock_Async_Context is null record;

   function Async_Send
     (Ctx  : in out Mock_Async_Context;
      Data : Byte_Array) return Natural
   is
      pragma Unreferenced (Ctx);
   begin
      if Async_Send_Fail then
         return 0;
      end if;
      for I in Data'Range loop
         Async_Send_Buffer (Async_Send_Length + I - Data'First) := Data (I);
      end loop;
      Async_Send_Length := Async_Send_Length + Data'Length;
      return Data'Length;
   end Async_Send;

   function Async_Receive
     (Ctx        : in out Mock_Async_Context;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural
   is
      pragma Unreferenced (Ctx, Timeout_Ms);
      Bytes_To_Copy : Natural;
   begin
      Buffer := [others => 0];
      if not Async_Recv_Available then
         return 0;
      end if;
      Bytes_To_Copy := Natural'Min (Max_Length, Async_Recv_Length - Async_Recv_Pos);
      for I in 0 .. Bytes_To_Copy - 1 loop
         Buffer (Buffer'First + I) := Async_Recv_Buffer (Async_Recv_Pos + I);
      end loop;
      Async_Recv_Pos := Async_Recv_Pos + Bytes_To_Copy;
      return Bytes_To_Copy;
   end Async_Receive;

   function Async_Get_Tick_Ms return Unsigned_32 is
   begin
      return Async_Time;
   end Async_Get_Tick_Ms;

   procedure Reset_Async_Mocks is
   begin
      Async_Send_Buffer := [others => 0];
      Async_Send_Length := 0;
      Async_Recv_Buffer := [others => 0];
      Async_Recv_Length := 0;
      Async_Recv_Pos := 0;
      Async_Time := 0;
      Async_Send_Fail := False;
      Async_Recv_Available := False;
      Callback_Called := False;
      Callback_Status := Success;
      Callback_Values := [others => 0];
      Callback_Value_Count := 0;
   end Reset_Async_Mocks;

   --  Instantiate Master with mock transport
   package Mock_Async_Master is new Ada_Modbus.Master
     (Transport_Context => Mock_Async_Context,
      Send              => Async_Send,
      Receive           => Async_Receive,
      Get_Tick_Ms       => Async_Get_Tick_Ms);

   --  Instantiate Async child package
   package Mock_Async is new Mock_Async_Master.Async
     (Max_Pending_Requests => 4);

   use type Mock_Async.Response_Status;
   use type Mock_Async.Request_Handle;

   Test_Async_Transport : Mock_Async_Context;

   --  Test callbacks
   procedure Test_Read_Callback
     (Handle         : Mock_Async.Request_Handle;
      Resp_Status    : Mock_Async.Response_Status;
      Slave          : Unit_Id;
      Values         : Register_Array;
      Exception_Code : Byte)
   is
      pragma Unreferenced (Handle, Slave, Exception_Code);
   begin
      Callback_Called := True;
      if Resp_Status = Mock_Async.Response_Success then
         Callback_Status := Success;
      elsif Resp_Status = Mock_Async.Response_Timeout then
         Callback_Status := Timeout;
      else
         Callback_Status := Frame_Error;
      end if;
      Callback_Value_Count := Values'Length;
      for I in Values'Range loop
         if I - Values'First < Callback_Values'Length then
            Callback_Values (I - Values'First) := Values (I);
         end if;
      end loop;
   end Test_Read_Callback;

   Write_Callback_Called : Boolean := False;
   Write_Callback_Status : Mock_Async.Response_Status := Mock_Async.Response_Success;

   procedure Test_Write_Callback
     (Handle         : Mock_Async.Request_Handle;
      Resp_Status    : Mock_Async.Response_Status;
      Slave          : Unit_Id;
      Exception_Code : Byte)
   is
      pragma Unreferenced (Handle, Slave, Exception_Code);
   begin
      Write_Callback_Called := True;
      Write_Callback_Status := Resp_Status;
   end Test_Write_Callback;

   type Async_Test_Case is new Test_Case with null record;

   overriding function Name (T : Async_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("Async Master Tests"));

   overriding procedure Register_Tests (T : in out Async_Test_Case);

   --  Test: Initialize async context
   procedure Test_Initialize (T : in Out Test_Case'Class);
   procedure Test_Initialize (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Master_Ctx : Mock_Async_Master.Master_Context;
      Config     : Mock_Async_Master.Master_Config;
      Async_Ctx  : Mock_Async.Async_Context;
   begin
      Reset_Async_Mocks;

      Config.Mode := Mock_Async_Master.RTU;
      Config.Default_Timeout := 1000;
      Mock_Async_Master.Initialize (Master_Ctx, Config, Test_Async_Transport);
      Mock_Async.Initialize (Async_Ctx, Master_Ctx);

      Assert (Mock_Async.Has_Free_Slot (Async_Ctx), "Should have free slots");
      Assert (Mock_Async.Pending_Count (Async_Ctx) = 0, "Should have 0 pending");
   end Test_Initialize;

   --  Test: Start async read request
   procedure Test_Start_Read_Async (T : in Out Test_Case'Class);
   procedure Test_Start_Read_Async (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Master_Ctx : Mock_Async_Master.Master_Context;
      Config     : Mock_Async_Master.Master_Config;
      Async_Ctx  : Mock_Async.Async_Context;
      Handle     : Mock_Async.Request_Handle;
      Success_Flag : Boolean;
   begin
      Reset_Async_Mocks;

      Config.Mode := Mock_Async_Master.RTU;
      Config.Default_Timeout := 1000;
      Mock_Async_Master.Initialize (Master_Ctx, Config, Test_Async_Transport);
      Mock_Async.Initialize (Async_Ctx, Master_Ctx);

      Success_Flag := Mock_Async.Read_Holding_Registers_Async
        (Async_Ctx,
         Slave         => 1,
         Start_Address => 0,
         Quantity      => 2,
         On_Response   => Test_Read_Callback'Access,
         Handle        => Handle);

      Assert (Success_Flag, "Should start async request");
      Assert (Mock_Async.Pending_Count (Async_Ctx) = 1, "Should have 1 pending");
      Assert (Async_Send_Length > 0, "Should have sent data");
   end Test_Start_Read_Async;

   --  Test: Process response
   procedure Test_Process_Response (T : in Out Test_Case'Class);
   procedure Test_Process_Response (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Master_Ctx : Mock_Async_Master.Master_Context;
      Config     : Mock_Async_Master.Master_Config;
      Async_Ctx  : Mock_Async.Async_Context;
      Handle     : Mock_Async.Request_Handle;
      Success_Flag : Boolean;
      Resp_PDU : Protocol.PDU_Buffer := [others => 0];
      Resp_ADU : Protocol.RTU.ADU_Buffer;
      Resp_Len : Natural;
   begin
      Reset_Async_Mocks;

      Config.Mode := Mock_Async_Master.RTU;
      Config.Default_Timeout := 1000;
      Mock_Async_Master.Initialize (Master_Ctx, Config, Test_Async_Transport);
      Mock_Async.Initialize (Async_Ctx, Master_Ctx);

      --  Start request
      Success_Flag := Mock_Async.Read_Holding_Registers_Async
        (Async_Ctx,
         Slave         => 1,
         Start_Address => 0,
         Quantity      => 2,
         On_Response   => Test_Read_Callback'Access,
         Handle        => Handle);

      Assert (Success_Flag, "Should start request");

      --  Prepare mock response
      Resp_PDU (0) := 16#03#;  --  FC
      Resp_PDU (1) := 4;       --  Byte count
      Resp_PDU (2) := 16#12#;
      Resp_PDU (3) := 16#34#;
      Resp_PDU (4) := 16#56#;
      Resp_PDU (5) := 16#78#;

      Protocol.RTU.Build_Frame (Resp_ADU, Resp_Len, Slave => 1,
                                PDU => Resp_PDU, PDU_Length => 6);

      for I in 0 .. Resp_Len - 1 loop
         Async_Recv_Buffer (I) := Resp_ADU (I);
      end loop;
      Async_Recv_Length := Resp_Len;
      Async_Recv_Available := True;

      --  Process pending - should invoke callback
      Mock_Async.Process_Pending (Async_Ctx);

      Assert (Callback_Called, "Callback should be called");
      Assert (Callback_Status = Ada_Modbus.Success, "Status should be Success");
      Assert (Callback_Value_Count = 2, "Should have 2 values");
      Assert (Callback_Values (0) = 16#1234#, "First value should be 0x1234");
      Assert (Callback_Values (1) = 16#5678#, "Second value should be 0x5678");
      Assert (Mock_Async.Pending_Count (Async_Ctx) = 0, "Should have 0 pending after response");
   end Test_Process_Response;

   --  Test: Timeout handling
   procedure Test_Async_Timeout (T : in Out Test_Case'Class);
   procedure Test_Async_Timeout (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Master_Ctx : Mock_Async_Master.Master_Context;
      Config     : Mock_Async_Master.Master_Config;
      Async_Ctx  : Mock_Async.Async_Context;
      Handle     : Mock_Async.Request_Handle;
      Success_Flag : Boolean;
   begin
      Reset_Async_Mocks;

      Config.Mode := Mock_Async_Master.RTU;
      Config.Default_Timeout := 100;  --  Short timeout
      Mock_Async_Master.Initialize (Master_Ctx, Config, Test_Async_Transport);
      Mock_Async.Initialize (Async_Ctx, Master_Ctx);

      Success_Flag := Mock_Async.Read_Holding_Registers_Async
        (Async_Ctx,
         Slave         => 1,
         Start_Address => 0,
         Quantity      => 2,
         On_Response   => Test_Read_Callback'Access,
         Handle        => Handle);

      Assert (Success_Flag, "Should start request");

      --  Simulate time passing beyond timeout
      Async_Time := 200;  --  200 ms elapsed

      --  No data available
      Async_Recv_Available := False;

      Mock_Async.Process_Pending (Async_Ctx);

      Assert (Callback_Called, "Callback should be called on timeout");
      Assert (Callback_Status = Timeout, "Status should be Timeout");
      Assert (Mock_Async.Pending_Count (Async_Ctx) = 0, "Should have 0 pending after timeout");
   end Test_Async_Timeout;

   --  Test: Cancel request
   procedure Test_Cancel_Request (T : in Out Test_Case'Class);
   procedure Test_Cancel_Request (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Master_Ctx : Mock_Async_Master.Master_Context;
      Config     : Mock_Async_Master.Master_Config;
      Async_Ctx  : Mock_Async.Async_Context;
      Handle     : Mock_Async.Request_Handle;
      Success_Flag : Boolean;
   begin
      Reset_Async_Mocks;

      Config.Mode := Mock_Async_Master.RTU;
      Config.Default_Timeout := 1000;
      Mock_Async_Master.Initialize (Master_Ctx, Config, Test_Async_Transport);
      Mock_Async.Initialize (Async_Ctx, Master_Ctx);

      Success_Flag := Mock_Async.Read_Holding_Registers_Async
        (Async_Ctx,
         Slave         => 1,
         Start_Address => 0,
         Quantity      => 2,
         On_Response   => Test_Read_Callback'Access,
         Handle        => Handle);

      Assert (Success_Flag, "Should start request");
      Assert (Mock_Async.Pending_Count (Async_Ctx) = 1, "Should have 1 pending");

      Mock_Async.Cancel_Request (Async_Ctx, Handle);

      Assert (Mock_Async.Pending_Count (Async_Ctx) = 0, "Should have 0 pending after cancel");
   end Test_Cancel_Request;

   --  Test: Write async
   procedure Test_Write_Async (T : in Out Test_Case'Class);
   procedure Test_Write_Async (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Master_Ctx : Mock_Async_Master.Master_Context;
      Config     : Mock_Async_Master.Master_Config;
      Async_Ctx  : Mock_Async.Async_Context;
      Handle     : Mock_Async.Request_Handle;
      Success_Flag : Boolean;
      Resp_PDU : Protocol.PDU_Buffer := [others => 0];
      Resp_ADU : Protocol.RTU.ADU_Buffer;
      Resp_Len : Natural;
   begin
      Reset_Async_Mocks;
      Write_Callback_Called := False;

      Config.Mode := Mock_Async_Master.RTU;
      Config.Default_Timeout := 1000;
      Mock_Async_Master.Initialize (Master_Ctx, Config, Test_Async_Transport);
      Mock_Async.Initialize (Async_Ctx, Master_Ctx);

      Success_Flag := Mock_Async.Write_Single_Register_Async
        (Async_Ctx,
         Slave       => 1,
         Address     => 10,
         Value       => 16#1234#,
         On_Response => Test_Write_Callback'Access,
         Handle      => Handle);

      Assert (Success_Flag, "Should start write request");

      --  Prepare mock echo response
      Resp_PDU (0) := 16#06#;
      Resp_PDU (1) := 16#00#;
      Resp_PDU (2) := 16#0A#;  --  Address 10
      Resp_PDU (3) := 16#12#;
      Resp_PDU (4) := 16#34#;  --  Value 0x1234

      Protocol.RTU.Build_Frame (Resp_ADU, Resp_Len, Slave => 1,
                                PDU => Resp_PDU, PDU_Length => 5);

      for I in 0 .. Resp_Len - 1 loop
         Async_Recv_Buffer (I) := Resp_ADU (I);
      end loop;
      Async_Recv_Length := Resp_Len;
      Async_Recv_Available := True;

      Mock_Async.Process_Pending (Async_Ctx);

      Assert (Write_Callback_Called, "Write callback should be called");
      Assert (Write_Callback_Status = Mock_Async.Response_Success, "Write should succeed");
   end Test_Write_Async;

   --  Test: Multiple pending requests
   procedure Test_Multiple_Pending (T : in Out Test_Case'Class);
   procedure Test_Multiple_Pending (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Master_Ctx : Mock_Async_Master.Master_Context;
      Config     : Mock_Async_Master.Master_Config;
      Async_Ctx  : Mock_Async.Async_Context;
      Handle1, Handle2, Handle3 : Mock_Async.Request_Handle;
      Success1, Success2, Success3 : Boolean;
   begin
      Reset_Async_Mocks;

      Config.Mode := Mock_Async_Master.RTU;
      Config.Default_Timeout := 1000;
      Mock_Async_Master.Initialize (Master_Ctx, Config, Test_Async_Transport);
      Mock_Async.Initialize (Async_Ctx, Master_Ctx);

      Success1 := Mock_Async.Read_Holding_Registers_Async
        (Async_Ctx, 1, 0, 1, Test_Read_Callback'Access, Handle1);
      Success2 := Mock_Async.Read_Holding_Registers_Async
        (Async_Ctx, 1, 10, 1, Test_Read_Callback'Access, Handle2);
      Success3 := Mock_Async.Read_Holding_Registers_Async
        (Async_Ctx, 1, 20, 1, Test_Read_Callback'Access, Handle3);

      Assert (Success1 and Success2 and Success3, "All requests should start");
      Assert (Mock_Async.Pending_Count (Async_Ctx) = 3, "Should have 3 pending");
      Assert (Handle1 /= Handle2 and Handle2 /= Handle3, "Handles should be different");
   end Test_Multiple_Pending;

   --  Test: Queue full
   procedure Test_Queue_Full (T : in Out Test_Case'Class);
   procedure Test_Queue_Full (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Master_Ctx : Mock_Async_Master.Master_Context;
      Config     : Mock_Async_Master.Master_Config;
      Async_Ctx  : Mock_Async.Async_Context;
      Handle     : Mock_Async.Request_Handle;
      Success_Flag : Boolean;
   begin
      Reset_Async_Mocks;

      Config.Mode := Mock_Async_Master.RTU;
      Config.Default_Timeout := 1000;
      Mock_Async_Master.Initialize (Master_Ctx, Config, Test_Async_Transport);
      Mock_Async.Initialize (Async_Ctx, Master_Ctx);

      --  Fill the queue (Max_Pending_Requests = 4)
      for I in 1 .. 4 loop
         Success_Flag := Mock_Async.Read_Holding_Registers_Async
           (Async_Ctx, 1, Register_Address (I), 1, Test_Read_Callback'Access, Handle);
         Assert (Success_Flag, "Request " & I'Image & " should succeed");
      end loop;

      Assert (Mock_Async.Pending_Count (Async_Ctx) = 4, "Should have 4 pending");
      Assert (not Mock_Async.Has_Free_Slot (Async_Ctx), "Should have no free slots");

      --  Try to add one more - should fail
      Success_Flag := Mock_Async.Read_Holding_Registers_Async
        (Async_Ctx, 1, 100, 1, Test_Read_Callback'Access, Handle);
      Assert (not Success_Flag, "5th request should fail - queue full");
   end Test_Queue_Full;

   --  Test: Read Input Registers Async (FC04)
   procedure Test_Read_Input_Registers_Async (T : in Out Test_Case'Class);
   procedure Test_Read_Input_Registers_Async (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Master_Ctx : Mock_Async_Master.Master_Context;
      Config     : Mock_Async_Master.Master_Config;
      Async_Ctx  : Mock_Async.Async_Context;
      Handle     : Mock_Async.Request_Handle;
      Success    : Boolean;
   begin
      Reset_Async_Mocks;

      Config.Mode := Mock_Async_Master.RTU;
      Config.Default_Timeout := 1000;
      Mock_Async_Master.Initialize (Master_Ctx, Config, Test_Async_Transport);
      Mock_Async.Initialize (Async_Ctx, Master_Ctx);

      Success := Mock_Async.Read_Input_Registers_Async
        (Async_Ctx, Slave => 1, Start_Address => 0, Quantity => 2,
         On_Response => Test_Read_Callback'Access, Handle => Handle);

      Assert (Success, "Read input registers async should succeed");
      Assert (Mock_Async.Pending_Count (Async_Ctx) = 1, "Should have 1 pending request");
   end Test_Read_Input_Registers_Async;

   --  Callback tracking for coils (set by callback, could be used for response testing)
   Coils_Callback_Called : Boolean := False with Unreferenced;

   procedure Test_Coils_Callback
     (Handle         : Mock_Async.Request_Handle;
      Resp_Status    : Mock_Async.Response_Status;
      Slave          : Unit_Id;
      Values         : Coil_Array;
      Exception_Code : Byte)
   is
      pragma Unreferenced (Handle, Resp_Status, Slave, Values, Exception_Code);
   begin
      Coils_Callback_Called := True;
   end Test_Coils_Callback;

   --  Test: Read Coils Async (FC01)
   procedure Test_Read_Coils_Async (T : in Out Test_Case'Class);
   procedure Test_Read_Coils_Async (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Master_Ctx : Mock_Async_Master.Master_Context;
      Config     : Mock_Async_Master.Master_Config;
      Async_Ctx  : Mock_Async.Async_Context;
      Handle     : Mock_Async.Request_Handle;
      Success    : Boolean;
   begin
      Reset_Async_Mocks;
      Coils_Callback_Called := False;

      Config.Mode := Mock_Async_Master.RTU;
      Config.Default_Timeout := 1000;
      Mock_Async_Master.Initialize (Master_Ctx, Config, Test_Async_Transport);
      Mock_Async.Initialize (Async_Ctx, Master_Ctx);

      Success := Mock_Async.Read_Coils_Async
        (Async_Ctx, Slave => 1, Start_Address => 0, Quantity => 8,
         On_Response => Test_Coils_Callback'Access, Handle => Handle);

      Assert (Success, "Read coils async should succeed");
      Assert (Mock_Async.Pending_Count (Async_Ctx) = 1, "Should have 1 pending request");
   end Test_Read_Coils_Async;

   --  Callback tracking for exception test
   Exception_Callback_Called : Boolean := False;
   Exception_Callback_Status : Mock_Async.Response_Status := Mock_Async.Response_Success;

   procedure Exception_Test_Callback
     (Handle         : Mock_Async.Request_Handle;
      Resp_Status    : Mock_Async.Response_Status;
      Slave          : Unit_Id;
      Values         : Register_Array;
      Exception_Code : Byte)
   is
      pragma Unreferenced (Handle, Slave, Values, Exception_Code);
   begin
      Exception_Callback_Called := True;
      Exception_Callback_Status := Resp_Status;
   end Exception_Test_Callback;

   --  Test: Exception Response Handling
   procedure Test_Exception_Response (T : in Out Test_Case'Class);
   procedure Test_Exception_Response (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Master_Ctx : Mock_Async_Master.Master_Context;
      Config     : Mock_Async_Master.Master_Config;
      Async_Ctx  : Mock_Async.Async_Context;
      Handle     : Mock_Async.Request_Handle;
      Resp_PDU   : Protocol.PDU_Buffer := [others => 0];
      Resp_ADU   : Protocol.RTU.ADU_Buffer;
      Resp_Len   : Natural;
      Success    : Boolean;
   begin
      Reset_Async_Mocks;
      Exception_Callback_Called := False;
      Exception_Callback_Status := Mock_Async.Response_Success;

      Config.Mode := Mock_Async_Master.RTU;
      Config.Default_Timeout := 1000;
      Mock_Async_Master.Initialize (Master_Ctx, Config, Test_Async_Transport);
      Mock_Async.Initialize (Async_Ctx, Master_Ctx);

      Success := Mock_Async.Read_Holding_Registers_Async
        (Async_Ctx, Slave => 1, Start_Address => 0, Quantity => 1,
         On_Response => Exception_Test_Callback'Access, Handle => Handle);
      Assert (Success, "Should start async read");

      --  Simulate exception response (FC + 0x80)
      Resp_PDU (0) := 16#83#;  --  FC03 + 0x80
      Resp_PDU (1) := 16#02#;  --  Illegal Address

      Protocol.RTU.Build_Frame (Resp_ADU, Resp_Len, Slave => 1, PDU => Resp_PDU, PDU_Length => 2);

      for I in 0 .. Resp_Len - 1 loop
         Async_Recv_Buffer (I) := Resp_ADU (I);
      end loop;
      Async_Recv_Length := Resp_Len;
      Async_Recv_Available := True;

      Mock_Async.Process_Pending (Async_Ctx);

      Assert (Exception_Callback_Called, "Callback should be invoked");
      Assert (Exception_Callback_Status = Mock_Async.Response_Exception, "Status should be Exception");
   end Test_Exception_Response;

   overriding procedure Register_Tests (T : in Out Async_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_Initialize'Access, "Initialize Async Context");
      Registration.Register_Routine (T, Test_Start_Read_Async'Access, "Start Read Async");
      Registration.Register_Routine (T, Test_Process_Response'Access, "Process Response");
      Registration.Register_Routine (T, Test_Async_Timeout'Access, "Async Timeout");
      Registration.Register_Routine (T, Test_Cancel_Request'Access, "Cancel Request");
      Registration.Register_Routine (T, Test_Write_Async'Access, "Write Async");
      Registration.Register_Routine (T, Test_Multiple_Pending'Access, "Multiple Pending");
      Registration.Register_Routine (T, Test_Queue_Full'Access, "Queue Full");
      Registration.Register_Routine (T, Test_Read_Input_Registers_Async'Access, "Read Input Registers Async");
      Registration.Register_Routine (T, Test_Read_Coils_Async'Access, "Read Coils Async");
      Registration.Register_Routine (T, Test_Exception_Response'Access, "Exception Response");
   end Register_Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new Async_Test_Case);
      return S;
   end Suite;

end Test_Async;
