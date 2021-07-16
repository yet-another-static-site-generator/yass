--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Messages.Test_Data.

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
package body Messages.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_ShowMessage_e90f4a_ce41f3
     (Text: String; MType: Messages_Types := Error) is
   begin
      begin
         pragma Assert(Text'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(messages.ads:0):Test_Show_Message test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Messages.ShowMessage(Text, MType);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(messages.ads:0:):Test_Show_Message test commitment violated");
      end;
   end Wrap_Test_ShowMessage_e90f4a_ce41f3;
--  end read only

--  begin read only
   procedure Test_ShowMessage_test_show_message(Gnattest_T: in out Test);
   procedure Test_ShowMessage_e90f4a_ce41f3(Gnattest_T: in out Test) renames
     Test_ShowMessage_test_show_message;
--  id:2.2/e90f4a52015b9be7/ShowMessage/1/0/test_show_message/
   procedure Test_ShowMessage_test_show_message(Gnattest_T: in out Test) is
      procedure ShowMessage
        (Text: String; MType: Messages_Types := Error) renames
        Wrap_Test_ShowMessage_e90f4a_ce41f3;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      ShowMessage("Test error message.");
      ShowMessage("Test normal message.", Normal);
      ShowMessage("Test success message.", Success);
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_ShowMessage_test_show_message;
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
end Messages.Test_Data.Tests;
