--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body Sitemaps.Test_Data.Tests.Suite is

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.Sitemaps.Test_Data.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_StartSitemap_351b30 : aliased Runner_1.Test_Case;
   Case_2_1_Test_AddPageToSitemap_afae92 : aliased Runner_1.Test_Case;
   Case_3_1_Test_SaveSitemap_8a2373 : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_StartSitemap_351b30,
         "sitemaps.ads:20:4:",
         Test_StartSitemap_351b30'Access);
      Runner_1.Create
        (Case_2_1_Test_AddPageToSitemap_afae92,
         "sitemaps.ads:21:4:",
         Test_AddPageToSitemap_afae92'Access);
      Runner_1.Create
        (Case_3_1_Test_SaveSitemap_8a2373,
         "sitemaps.ads:24:4:",
         Test_SaveSitemap_8a2373'Access);

      Result.Add_Test (Case_1_1_Test_StartSitemap_351b30'Access);
      Result.Add_Test (Case_2_1_Test_AddPageToSitemap_afae92'Access);
      Result.Add_Test (Case_3_1_Test_SaveSitemap_8a2373'Access);

      return Result'Access;

   end Suite;

end Sitemaps.Test_Data.Tests.Suite;
--  end read only
