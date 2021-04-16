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
   function Wrap_Test_To_Time_e953f5_8a629f(Date: String) return Time is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(atomfeed.ads:0):Test_To_Date test requirement violated");
      end;
      declare
         Test_To_Time_e953f5_8a629f_Result: constant Time :=
           GNATtest_Generated.GNATtest_Standard.AtomFeed.To_Time(Date);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(atomfeed.ads:0:):Test_To_Date test commitment violated");
         end;
         return Test_To_Time_e953f5_8a629f_Result;
      end;
   end Wrap_Test_To_Time_e953f5_8a629f;
--  end read only

--  begin read only
   procedure Test_To_Time_test_to_date(Gnattest_T: in out Test);
   procedure Test_To_Time_e953f5_8a629f(Gnattest_T: in out Test) renames
     Test_To_Time_test_to_date;
--  id:2.2/e953f5f4ae1398fb/To_Time/1/0/test_to_date/
   procedure Test_To_Time_test_to_date(Gnattest_T: in out Test) is
      function To_Time(Date: String) return Time renames
        Wrap_Test_To_Time_e953f5_8a629f;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (To_Time("2019-03-03T04:12:45Z") = Value("2019-03-03 04:12:45"),
         "Invalid HTTP date to Ada date convertion.");
      Assert
        (To_Time("2019-03-03") = Value("2019-03-03 00:00:00"),
         "Incomplete HTTP date to Ada date not converted.");

--  begin read only
   end Test_To_Time_test_to_date;
--  end read only

--  begin read only
   function Wrap_Test_To_HTTP_Date_2bd5b1_bcc90f(Date: Time) return String is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(atomfeed.ads:0):Test_To_HTTP_Date test requirement violated");
      end;
      declare
         Test_To_HTTP_Date_2bd5b1_bcc90f_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.AtomFeed.To_HTTP_Date(Date);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(atomfeed.ads:0:):Test_To_HTTP_Date test commitment violated");
         end;
         return Test_To_HTTP_Date_2bd5b1_bcc90f_Result;
      end;
   end Wrap_Test_To_HTTP_Date_2bd5b1_bcc90f;
--  end read only

--  begin read only
   procedure Test_To_HTTP_Date_test_to_http_date(Gnattest_T: in out Test);
   procedure Test_To_HTTP_Date_2bd5b1_bcc90f(Gnattest_T: in out Test) renames
     Test_To_HTTP_Date_test_to_http_date;
--  id:2.2/2bd5b10ba8625102/To_HTTP_Date/1/0/test_to_http_date/
   procedure Test_To_HTTP_Date_test_to_http_date(Gnattest_T: in out Test) is
      function To_HTTP_Date(Date: Time) return String renames
        Wrap_Test_To_HTTP_Date_2bd5b1_bcc90f;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (To_HTTP_Date(Value("2019-03-03 04:12:45")) = "2019-03-03T04:12:45Z",
         "Invalid Ada date to HTTP date convertion.");

--  begin read only
   end Test_To_HTTP_Date_test_to_http_date;
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
