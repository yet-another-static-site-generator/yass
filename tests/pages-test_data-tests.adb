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
   procedure Wrap_Test_Create_Page_93a400_dc01ab
     (File_Name, Directory: String) is
   begin
      begin
         pragma Assert(File_Name'Length > 0 and Directory'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(pages.ads:0):Test_Create_Page test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Pages.Create_Page
        (File_Name, Directory);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(pages.ads:0:):Test_Create_Page test commitment violated");
      end;
   end Wrap_Test_Create_Page_93a400_dc01ab;
--  end read only

--  begin read only
   procedure Test_Create_Page_test_create_page(Gnattest_T: in out Test);
   procedure Test_Create_Page_93a400_dc01ab(Gnattest_T: in out Test) renames
     Test_Create_Page_test_create_page;
--  id:2.2/93a4003e12046daf/Create_Page/1/0/test_create_page/
   procedure Test_Create_Page_test_create_page(Gnattest_T: in out Test) is
      procedure Create_Page(File_Name, Directory: String) renames
        Wrap_Test_Create_Page_93a400_dc01ab;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Start_Sitemap;
      Create_Empty_File("test.md");
      Create_Page("test.md", ".");
      Assert
        (Exists("_output/test.html"),
         "Failed to create a new HTML page from Markdown file.");

--  begin read only
   end Test_Create_Page_test_create_page;
--  end read only

--  begin read only
   procedure Wrap_Test_Copy_File_2f1aa9_049492(File_Name, Directory: String) is
   begin
      begin
         pragma Assert(File_Name'Length > 0 and Directory'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(pages.ads:0):Test_Copy_File test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Pages.Copy_File
        (File_Name, Directory);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(pages.ads:0:):Test_Copy_File test commitment violated");
      end;
   end Wrap_Test_Copy_File_2f1aa9_049492;
--  end read only

--  begin read only
   procedure Test_Copy_File_test_copy_file(Gnattest_T: in out Test);
   procedure Test_Copy_File_2f1aa9_049492(Gnattest_T: in out Test) renames
     Test_Copy_File_test_copy_file;
--  id:2.2/2f1aa9959af2dee0/Copy_File/1/0/test_copy_file/
   procedure Test_Copy_File_test_copy_file(Gnattest_T: in out Test) is
      procedure Copy_File(File_Name, Directory: String) renames
        Wrap_Test_Copy_File_2f1aa9_049492;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Pages.Copy_File("test.md", ".");
      Assert
        (Exists("_output/test.md"),
         "Failed to copy file to output directory.");

--  begin read only
   end Test_Copy_File_test_copy_file;
--  end read only

--  begin read only
   procedure Wrap_Test_Create_Empty_File_07d252_44c53f(File_Name: String) is
   begin
      begin
         pragma Assert(File_Name'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(pages.ads:0):Test_Create_Empty_File test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Pages.Create_Empty_File(File_Name);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(pages.ads:0:):Test_Create_Empty_File test commitment violated");
      end;
   end Wrap_Test_Create_Empty_File_07d252_44c53f;
--  end read only

--  begin read only
   procedure Test_Create_Empty_File_test_create_empty_file
     (Gnattest_T: in out Test);
   procedure Test_Create_Empty_File_07d252_44c53f
     (Gnattest_T: in out Test) renames
     Test_Create_Empty_File_test_create_empty_file;
--  id:2.2/07d2529fa48cbec9/Create_Empty_File/1/0/test_create_empty_file/
   procedure Test_Create_Empty_File_test_create_empty_file
     (Gnattest_T: in out Test) is
      procedure Create_Empty_File(File_Name: String) renames
        Wrap_Test_Create_Empty_File_07d252_44c53f;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Create_Empty_File("mynewfile.md");
      Assert(Exists("mynewfile.md"), "Failed to create empty Markdown file.");
      Delete_File("mynewfile.md");

--  begin read only
   end Test_Create_Empty_File_test_create_empty_file;
--  end read only

--  begin read only
   function Wrap_Test_Get_Layout_Name_dd1d05_287311
     (File_Name: String) return String is
   begin
      begin
         pragma Assert(File_Name'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(pages.ads:0):Test_Get_Layout_Name test requirement violated");
      end;
      declare
         Test_Get_Layout_Name_dd1d05_287311_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Pages.Get_Layout_Name
             (File_Name);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(pages.ads:0:):Test_Get_Layout_Name test commitment violated");
         end;
         return Test_Get_Layout_Name_dd1d05_287311_Result;
      end;
   end Wrap_Test_Get_Layout_Name_dd1d05_287311;
--  end read only

--  begin read only
   procedure Test_Get_Layout_Name_test_get_layout_name
     (Gnattest_T: in out Test);
   procedure Test_Get_Layout_Name_dd1d05_287311
     (Gnattest_T: in out Test) renames
     Test_Get_Layout_Name_test_get_layout_name;
--  id:2.2/dd1d05e38a591979/Get_Layout_Name/1/0/test_get_layout_name/
   procedure Test_Get_Layout_Name_test_get_layout_name
     (Gnattest_T: in out Test) is
      function Get_Layout_Name(File_Name: String) return String renames
        Wrap_Test_Get_Layout_Name_dd1d05_287311;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Get_Layout_Name("test.md") = "./_layouts/default.html",
         "Failed to get the path to the selected layout.");

--  begin read only
   end Test_Get_Layout_Name_test_get_layout_name;
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
