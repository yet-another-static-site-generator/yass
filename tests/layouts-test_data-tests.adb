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
   procedure Test_CreateLayout (Gnattest_T : in out Test);
   procedure Test_CreateLayout_376911 (Gnattest_T : in out Test) renames Test_CreateLayout;
--  id:2.2/3769116bf305c9ae/CreateLayout/1/0/
   procedure Test_CreateLayout (Gnattest_T : in out Test) is
   --  layouts.ads:20:4:CreateLayout
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_CreateLayout;
--  end read only


--  begin read only
   procedure Test_CreateDirectoryLayout (Gnattest_T : in out Test);
   procedure Test_CreateDirectoryLayout_0a44a9 (Gnattest_T : in out Test) renames Test_CreateDirectoryLayout;
--  id:2.2/0a44a90ea4d17865/CreateDirectoryLayout/1/0/
   procedure Test_CreateDirectoryLayout (Gnattest_T : in out Test) is
   --  layouts.ads:22:4:CreateDirectoryLayout
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_CreateDirectoryLayout;
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
