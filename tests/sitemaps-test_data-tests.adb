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
   procedure Wrap_Test_Start_Sitemap_8927d0_223cc9 is
   begin
      GNATtest_Generated.GNATtest_Standard.Sitemaps.Start_Sitemap;
   end Wrap_Test_Start_Sitemap_8927d0_223cc9;
--  end read only

--  begin read only
   procedure Test_Start_Sitemap_test_start_sitemap(Gnattest_T: in out Test);
   procedure Test_Start_Sitemap_8927d0_223cc9(Gnattest_T: in out Test) renames
     Test_Start_Sitemap_test_start_sitemap;
--  id:2.2/8927d00e0fdf45e6/Start_Sitemap/1/0/test_start_sitemap/
   procedure Test_Start_Sitemap_test_start_sitemap(Gnattest_T: in out Test) is
      procedure Start_Sitemap renames Wrap_Test_Start_Sitemap_8927d0_223cc9;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Start_Sitemap;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Start_Sitemap_test_start_sitemap;
--  end read only

--  begin read only
   procedure Wrap_Test_Add_Page_To_Sitemap_3dbb1d_d33b30
     (File_Name, Change_Frequency, Page_Priority: String) is
   begin
      GNATtest_Generated.GNATtest_Standard.Sitemaps.Add_Page_To_Sitemap
        (File_Name, Change_Frequency, Page_Priority);
   end Wrap_Test_Add_Page_To_Sitemap_3dbb1d_d33b30;
--  end read only

--  begin read only
   procedure Test_Add_Page_To_Sitemap_test_add_page_to_sitemap
     (Gnattest_T: in out Test);
   procedure Test_Add_Page_To_Sitemap_3dbb1d_d33b30
     (Gnattest_T: in out Test) renames
     Test_Add_Page_To_Sitemap_test_add_page_to_sitemap;
--  id:2.2/3dbb1dd1e8c223e0/Add_Page_To_Sitemap/1/0/test_add_page_to_sitemap/
   procedure Test_Add_Page_To_Sitemap_test_add_page_to_sitemap
     (Gnattest_T: in out Test) is
      procedure Add_Page_To_Sitemap
        (File_Name, Change_Frequency, Page_Priority: String) renames
        Wrap_Test_Add_Page_To_Sitemap_3dbb1d_d33b30;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Add_Page_To_Sitemap("mypage.html", "", "");
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Add_Page_To_Sitemap_test_add_page_to_sitemap;
--  end read only

--  begin read only
   procedure Wrap_Test_Save_Sitemap_ede18d_7f961d is
   begin
      GNATtest_Generated.GNATtest_Standard.Sitemaps.Save_Sitemap;
   end Wrap_Test_Save_Sitemap_ede18d_7f961d;
--  end read only

--  begin read only
   procedure Test_Save_Sitemap_test_save_sitemap(Gnattest_T: in out Test);
   procedure Test_Save_Sitemap_ede18d_7f961d(Gnattest_T: in out Test) renames
     Test_Save_Sitemap_test_save_sitemap;
--  id:2.2/ede18d12831cc9b2/Save_Sitemap/1/0/test_save_sitemap/
   procedure Test_Save_Sitemap_test_save_sitemap(Gnattest_T: in out Test) is
      procedure Save_Sitemap renames Wrap_Test_Save_Sitemap_ede18d_7f961d;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Save_Sitemap;
      Assert
        (Exists
           (To_String(Yass_Config.Output_Directory) & Dir_Separator &
            "sitemap.xml"),
         "Failed to create the sitemap file.");

--  begin read only
   end Test_Save_Sitemap_test_save_sitemap;
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
