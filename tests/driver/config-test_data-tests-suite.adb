--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body Config.Test_Data.Tests.Suite is

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.Config.Test_Data.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_CreateConfig_dacee6 : aliased Runner_1.Test_Case;
   Case_2_1_Test_ParseConfig_4edbef : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_CreateConfig_dacee6,
         "config.ads:85:4:",
         Test_CreateConfig_dacee6'Access);
      Runner_1.Create
        (Case_2_1_Test_ParseConfig_4edbef,
         "config.ads:87:4:",
         Test_ParseConfig_4edbef'Access);

      Result.Add_Test (Case_1_1_Test_CreateConfig_dacee6'Access);
      Result.Add_Test (Case_2_1_Test_ParseConfig_4edbef'Access);

      return Result'Access;

   end Suite;

end Config.Test_Data.Tests.Suite;
--  end read only
