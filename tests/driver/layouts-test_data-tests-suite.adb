--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body Layouts.Test_Data.Tests.Suite is

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.Layouts.Test_Data.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_CreateLayout_376911 : aliased Runner_1.Test_Case;
   Case_2_1_Test_CreateDirectoryLayout_0a44a9 : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_CreateLayout_376911,
         "layouts.ads:20:4:",
         Test_CreateLayout_376911'Access);
      Runner_1.Create
        (Case_2_1_Test_CreateDirectoryLayout_0a44a9,
         "layouts.ads:22:4:",
         Test_CreateDirectoryLayout_0a44a9'Access);

      Result.Add_Test (Case_1_1_Test_CreateLayout_376911'Access);
      Result.Add_Test (Case_2_1_Test_CreateDirectoryLayout_0a44a9'Access);

      return Result'Access;

   end Suite;

end Layouts.Test_Data.Tests.Suite;
--  end read only
