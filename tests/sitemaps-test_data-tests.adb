--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Sitemaps.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Config; use Config;

--  begin read only
--  end read only
package body Sitemaps.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_StartSitemap_351b30_223cc9 is
   begin
      GNATtest_Generated.GNATtest_Standard.Sitemaps.StartSitemap;
   end Wrap_Test_StartSitemap_351b30_223cc9;
--  end read only

--  begin read only
   procedure Test_StartSitemap_test_start_sitemap(Gnattest_T: in out Test);
   procedure Test_StartSitemap_351b30_223cc9(Gnattest_T: in out Test) renames
     Test_StartSitemap_test_start_sitemap;
--  id:2.2/351b309d3c5a5237/StartSitemap/1/0/test_start_sitemap/
   procedure Test_StartSitemap_test_start_sitemap(Gnattest_T: in out Test) is
      procedure StartSitemap renames Wrap_Test_StartSitemap_351b30_223cc9;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      StartSitemap;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_StartSitemap_test_start_sitemap;
--  end read only

--  begin read only
   procedure Wrap_Test_AddPageToSitemap_afae92_d33b30
     (FileName, ChangeFrequency, PagePriority: String) is
   begin
      GNATtest_Generated.GNATtest_Standard.Sitemaps.AddPageToSitemap
        (FileName, ChangeFrequency, PagePriority);
   end Wrap_Test_AddPageToSitemap_afae92_d33b30;
--  end read only

--  begin read only
   procedure Test_AddPageToSitemap_test_add_page_to_sitemap
     (Gnattest_T: in out Test);
   procedure Test_AddPageToSitemap_afae92_d33b30
     (Gnattest_T: in out Test) renames
     Test_AddPageToSitemap_test_add_page_to_sitemap;
--  id:2.2/afae92cfb7a27b84/AddPageToSitemap/1/0/test_add_page_to_sitemap/
   procedure Test_AddPageToSitemap_test_add_page_to_sitemap
     (Gnattest_T: in out Test) is
      procedure AddPageToSitemap
        (FileName, ChangeFrequency, PagePriority: String) renames
        Wrap_Test_AddPageToSitemap_afae92_d33b30;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      AddPageToSitemap("mypage.html", "", "");
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_AddPageToSitemap_test_add_page_to_sitemap;
--  end read only

--  begin read only
   procedure Wrap_Test_SaveSitemap_8a2373_7f961d is
   begin
      GNATtest_Generated.GNATtest_Standard.Sitemaps.SaveSitemap;
   end Wrap_Test_SaveSitemap_8a2373_7f961d;
--  end read only

--  begin read only
   procedure Test_SaveSitemap_test_save_sitemap(Gnattest_T: in out Test);
   procedure Test_SaveSitemap_8a2373_7f961d(Gnattest_T: in out Test) renames
     Test_SaveSitemap_test_save_sitemap;
--  id:2.2/8a2373146aca9803/SaveSitemap/1/0/test_save_sitemap/
   procedure Test_SaveSitemap_test_save_sitemap(Gnattest_T: in out Test) is
      procedure SaveSitemap renames Wrap_Test_SaveSitemap_8a2373_7f961d;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      SaveSitemap;
      Assert
        (Exists
           (To_String(Yass_Config.Output_Directory) & Dir_Separator &
            "sitemap.xml"), "Failed to create the sitemap file.");

--  begin read only
   end Test_SaveSitemap_test_save_sitemap;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Sitemaps.Test_Data.Tests;
