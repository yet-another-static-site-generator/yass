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
   procedure Wrap_Test_Show_Message_b0ef3e_268f83
     (Text: String; Message_Type: Messages_Types := Default_Message_Type) is
   begin
      begin
         pragma Assert(Text'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(messages.ads:0:):Test_Show_Message test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Messages.Show_Message
        (Text, Message_Type);
   end Wrap_Test_Show_Message_b0ef3e_268f83;
--  end read only

--  begin read only
   procedure Test_Show_Message_test_show_message(Gnattest_T: in out Test);
   procedure Test_Show_Message_b0ef3e_268f83(Gnattest_T: in out Test) renames
     Test_Show_Message_test_show_message;
--  id:2.2/b0ef3ea444439e3e/Show_Message/1/0/test_show_message/
   procedure Test_Show_Message_test_show_message(Gnattest_T: in out Test) is
      procedure Show_Message
        (Text: String;
         Message_Type: Messages_Types := Default_Message_Type) renames
        Wrap_Test_Show_Message_b0ef3e_268f83;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Show_Message("Test error message.");
      Show_Message("Test normal message.", NORMAL);
      Show_Message("Test success message.", SUCCESS);
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Show_Message_test_show_message;
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
