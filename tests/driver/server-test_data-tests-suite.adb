--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body Server.Test_Data.Tests.Suite is

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.Server.Test_Data.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_Callback_f59e91 : aliased Runner_1.Test_Case;
   Case_2_1_Test_StartServer_1470b4 : aliased Runner_1.Test_Case;
   Case_3_1_Test_ShutdownServer_f63a3b : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_Callback_f59e91,
         "server.ads:31:4:",
         Test_Callback_f59e91'Access);
      Runner_1.Create
        (Case_2_1_Test_StartServer_1470b4,
         "server.ads:34:4:",
         Test_StartServer_1470b4'Access);
      Runner_1.Create
        (Case_3_1_Test_ShutdownServer_f63a3b,
         "server.ads:35:4:",
         Test_ShutdownServer_f63a3b'Access);

      Result.Add_Test (Case_1_1_Test_Callback_f59e91'Access);
      Result.Add_Test (Case_2_1_Test_StartServer_1470b4'Access);
      Result.Add_Test (Case_3_1_Test_ShutdownServer_f63a3b'Access);

      return Result'Access;

   end Suite;

end Server.Test_Data.Tests.Suite;
--  end read only
