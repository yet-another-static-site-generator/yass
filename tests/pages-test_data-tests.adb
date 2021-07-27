--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pages.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ada.Directories; use Ada.Directories;
with Config; use Config;
with Sitemaps; use Sitemaps;

--  begin read only
--  end read only
package body Pages.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_CreatePage_8e91e0_6a6713(FileName, Directory: String) is
   begin
      begin
         pragma Assert(FileName'Length > 0 and Directory'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(pages.ads:0):Test_Create_Page test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Pages.CreatePage
        (FileName, Directory);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(pages.ads:0:):Test_Create_Page test commitment violated");
      end;
   end Wrap_Test_CreatePage_8e91e0_6a6713;
--  end read only

--  begin read only
   procedure Test_CreatePage_test_create_page(Gnattest_T: in out Test);
   procedure Test_CreatePage_8e91e0_6a6713(Gnattest_T: in out Test) renames
     Test_CreatePage_test_create_page;
--  id:2.2/8e91e0bf6f60a68d/CreatePage/1/0/test_create_page/
   procedure Test_CreatePage_test_create_page(Gnattest_T: in out Test) is
      procedure CreatePage(FileName, Directory: String) renames
        Wrap_Test_CreatePage_8e91e0_6a6713;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      StartSitemap;
      CreateEmptyFile("test.md");
      CreatePage("test.md", ".");
      Assert
        (Exists("_output/test.html"),
         "Failed to create a new HTML page from Markdown file.");

--  begin read only
   end Test_CreatePage_test_create_page;
--  end read only

--  begin read only
   procedure Wrap_Test_CopyFile_1a0ae8_88695c(FileName, Directory: String) is
   begin
      begin
         pragma Assert(FileName'Length > 0 and Directory'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(pages.ads:0):Test_Copy_File test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Pages.CopyFile(FileName, Directory);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(pages.ads:0:):Test_Copy_File test commitment violated");
      end;
   end Wrap_Test_CopyFile_1a0ae8_88695c;
--  end read only

--  begin read only
   procedure Test_CopyFile_test_copy_file(Gnattest_T: in out Test);
   procedure Test_CopyFile_1a0ae8_88695c(Gnattest_T: in out Test) renames
     Test_CopyFile_test_copy_file;
--  id:2.2/1a0ae87d2933d598/CopyFile/1/0/test_copy_file/
   procedure Test_CopyFile_test_copy_file(Gnattest_T: in out Test) is
      procedure CopyFile(FileName, Directory: String) renames
        Wrap_Test_CopyFile_1a0ae8_88695c;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      CopyFile("test.md", ".");
      Assert
        (Exists("_output/test.md"),
         "Failed to copy file to output directory.");

--  begin read only
   end Test_CopyFile_test_copy_file;
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
end Pages.Test_Data.Tests;
