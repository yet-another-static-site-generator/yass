--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into AtomFeed.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
--  begin read only
--  end read only
package body AtomFeed.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_To_Time (Gnattest_T : in out Test);
   procedure Test_To_Time_e953f5 (Gnattest_T : in out Test) renames Test_To_Time;
--  id:2.2/e953f5f4ae1398fb/To_Time/1/0/
   procedure Test_To_Time (Gnattest_T : in out Test) is
   --  atomfeed.ads:36:4:To_Time
--  end read only

      pragma Unreferenced (Gnattest_T);
   begin
      Assert(To_Time("2019-03-03T04:12:45Z") = Value("2019-03-03 04:12:45"),
         "Invalid HTTP date to Ada date convertion.");
      Assert(To_Time("2019-03-03") = Value("2019-03-03 00:00:00"),
         "Incomplete HTTP date to Ada date not converted.");
--  begin read only
   end Test_To_Time;
--  end read only


--  begin read only
   procedure Test_To_HTTP_Date (Gnattest_T : in out Test);
   procedure Test_To_HTTP_Date_2bd5b1 (Gnattest_T : in out Test) renames Test_To_HTTP_Date;
--  id:2.2/2bd5b10ba8625102/To_HTTP_Date/1/0/
   procedure Test_To_HTTP_Date (Gnattest_T : in out Test) is
   --  atomfeed.ads:38:4:To_HTTP_Date
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_To_HTTP_Date;
--  end read only


--  begin read only
   procedure Test_StartAtomFeed (Gnattest_T : in out Test);
   procedure Test_StartAtomFeed_57b23e (Gnattest_T : in out Test) renames Test_StartAtomFeed;
--  id:2.2/57b23eba36eee842/StartAtomFeed/1/0/
   procedure Test_StartAtomFeed (Gnattest_T : in out Test) is
   --  atomfeed.ads:40:4:StartAtomFeed
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_StartAtomFeed;
--  end read only


--  begin read only
   procedure Test_AddPageToFeed (Gnattest_T : in out Test);
   procedure Test_AddPageToFeed_6edf8c (Gnattest_T : in out Test) renames Test_AddPageToFeed;
--  id:2.2/6edf8cf85ed5d38f/AddPageToFeed/1/0/
   procedure Test_AddPageToFeed (Gnattest_T : in out Test) is
   --  atomfeed.ads:41:4:AddPageToFeed
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_AddPageToFeed;
--  end read only


--  begin read only
   procedure Test_SaveAtomFeed (Gnattest_T : in out Test);
   procedure Test_SaveAtomFeed_b0372c (Gnattest_T : in out Test) renames Test_SaveAtomFeed;
--  id:2.2/b0372c6aa48ce7f2/SaveAtomFeed/1/0/
   procedure Test_SaveAtomFeed (Gnattest_T : in out Test) is
   --  atomfeed.ads:44:4:SaveAtomFeed
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_SaveAtomFeed;
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
end AtomFeed.Test_Data.Tests;
