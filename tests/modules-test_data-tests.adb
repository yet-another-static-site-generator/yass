--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Modules.Test_Data.

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
package body Modules.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Load_Modules_944a26_495a16
     (State: String; Page_Tags: in out Tags_Container.Map;
      Page_Table_Tags: in out TableTags_Container.Map) is
   begin
      begin
         pragma Assert(State'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(modules.ads:0:):Test_Load_Modules test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Modules.Load_Modules
        (State, Page_Tags, Page_Table_Tags);
   end Wrap_Test_Load_Modules_944a26_495a16;
--  end read only

--  begin read only
   procedure Test_Load_Modules_test_load_modules(Gnattest_T: in out Test);
   procedure Test_Load_Modules_944a26_495a16(Gnattest_T: in out Test) renames
     Test_Load_Modules_test_load_modules;
--  id:2.2/944a2653cbe1fa78/Load_Modules/1/0/test_load_modules/
   procedure Test_Load_Modules_test_load_modules(Gnattest_T: in out Test) is
      procedure Load_Modules
        (State: String; Page_Tags: in out Tags_Container.Map;
         Page_Table_Tags: in out TableTags_Container.Map) renames
        Wrap_Test_Load_Modules_944a26_495a16;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Page_Tags: Tags_Container.Map := Tags_Container.Empty_Map;
      Page_Table_Tags: TableTags_Container.Map :=
        TableTags_Container.Empty_Map;

   begin

      Load_Modules("start", Page_Tags, Page_Table_Tags);
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Load_Modules_test_load_modules;
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
end Modules.Test_Data.Tests;
