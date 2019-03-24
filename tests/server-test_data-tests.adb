--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Server.Test_Data.

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
package body Server.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_StartServer (Gnattest_T : in out Test);
   procedure Test_StartServer_1470b4 (Gnattest_T : in out Test) renames Test_StartServer;
--  id:2.2/1470b4fce0379142/StartServer/1/0/
   procedure Test_StartServer (Gnattest_T : in out Test) is
   --  server.ads:28:4:StartServer
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_StartServer;
--  end read only


--  begin read only
   procedure Test_ShutdownServer (Gnattest_T : in out Test);
   procedure Test_ShutdownServer_f63a3b (Gnattest_T : in out Test) renames Test_ShutdownServer;
--  id:2.2/f63a3b9e26d7ee05/ShutdownServer/1/0/
   procedure Test_ShutdownServer (Gnattest_T : in out Test) is
   --  server.ads:29:4:ShutdownServer
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_ShutdownServer;
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
end Server.Test_Data.Tests;
