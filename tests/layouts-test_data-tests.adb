--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Layouts.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ada.Directories; use Ada.Directories;

--  begin read only
--  end read only
package body Layouts.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Create_Layout_c5b31b_3f7438(Directory_Name: String) is
   begin
      begin
         pragma Assert(Directory_Name'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(layouts.ads:0:):Test_Create_Layout test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Layouts.Create_Layout
        (Directory_Name);
   end Wrap_Test_Create_Layout_c5b31b_3f7438;
--  end read only

--  begin read only
   procedure Test_Create_Layout_test_create_layout(Gnattest_T: in out Test);
   procedure Test_Create_Layout_c5b31b_3f7438(Gnattest_T: in out Test) renames
     Test_Create_Layout_test_create_layout;
--  id:2.2/c5b31b74a47d8c7a/Create_Layout/1/0/test_create_layout/
   procedure Test_Create_Layout_test_create_layout(Gnattest_T: in out Test) is
      procedure Create_Layout(Directory_Name: String) renames
        Wrap_Test_Create_Layout_c5b31b_3f7438;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Create_Path("_layouts");
      Create_Layout(".");
      Assert
        (Exists("_layouts/default.html"),
         "Failed to create the default layout for the project.");

--  begin read only
   end Test_Create_Layout_test_create_layout;
--  end read only

--  begin read only
   procedure Wrap_Test_Create_Directory_Layout_00199c_0f2a92
     (Directory_Name: String) is
   begin
      GNATtest_Generated.GNATtest_Standard.Layouts.Create_Directory_Layout
        (Directory_Name);
   end Wrap_Test_Create_Directory_Layout_00199c_0f2a92;
--  end read only

--  begin read only
   procedure Test_Create_Directory_Layout_test_create_directory_layout
     (Gnattest_T: in out Test);
   procedure Test_Create_Directory_Layout_00199c_0f2a92
     (Gnattest_T: in out Test) renames
     Test_Create_Directory_Layout_test_create_directory_layout;
--  id:2.2/00199c94c82a9cc4/Create_Directory_Layout/1/0/test_create_directory_layout/
   procedure Test_Create_Directory_Layout_test_create_directory_layout
     (Gnattest_T: in out Test) is
      procedure Create_Directory_Layout(Directory_Name: String) renames
        Wrap_Test_Create_Directory_Layout_00199c_0f2a92;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Create_Directory_Layout(".");
      Assert
        (Exists("_layouts/directory.html"),
         "Failed to create the default directory layout for the project.");

--  begin read only
   end Test_Create_Directory_Layout_test_create_directory_layout;
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
end Layouts.Test_Data.Tests;
