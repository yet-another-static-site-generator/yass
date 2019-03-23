--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body Pages.Test_Data.Tests.Suite is

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.Pages.Test_Data.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_CreatePage_8e91e0 : aliased Runner_1.Test_Case;
   Case_2_1_Test_CopyFile_1a0ae8 : aliased Runner_1.Test_Case;
   Case_3_1_Test_CreateEmptyFile_7571dc : aliased Runner_1.Test_Case;
   Case_4_1_Test_GetLayoutName_5a4eaf : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_CreatePage_8e91e0,
         "pages.ads:22:4:",
         Test_CreatePage_8e91e0'Access);
      Runner_1.Create
        (Case_2_1_Test_CopyFile_1a0ae8,
         "pages.ads:24:4:",
         Test_CopyFile_1a0ae8'Access);
      Runner_1.Create
        (Case_3_1_Test_CreateEmptyFile_7571dc,
         "pages.ads:27:4:",
         Test_CreateEmptyFile_7571dc'Access);
      Runner_1.Create
        (Case_4_1_Test_GetLayoutName_5a4eaf,
         "pages.ads:29:4:",
         Test_GetLayoutName_5a4eaf'Access);

      Result.Add_Test (Case_1_1_Test_CreatePage_8e91e0'Access);
      Result.Add_Test (Case_2_1_Test_CopyFile_1a0ae8'Access);
      Result.Add_Test (Case_3_1_Test_CreateEmptyFile_7571dc'Access);
      Result.Add_Test (Case_4_1_Test_GetLayoutName_5a4eaf'Access);

      return Result'Access;

   end Suite;

end Pages.Test_Data.Tests.Suite;
--  end read only
