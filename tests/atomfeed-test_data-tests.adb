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
with Ada.Directories; use Ada.Directories;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Config; use Config;

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
   function Wrap_Test_To_Time_e953f5_b6f0f8(Date: String) return Time is
   begin
      begin
         pragma Assert(Date'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(atomfeed.ads:0):Test_To_Date test requirement violated");
      end;
      declare
         Test_To_Time_e953f5_b6f0f8_Result: constant Time :=
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
         return Test_To_Time_e953f5_b6f0f8_Result;
      end;
   end Wrap_Test_To_Time_e953f5_b6f0f8;
--  end read only

--  begin read only
   procedure Test_To_Time_test_to_date(Gnattest_T: in out Test);
   procedure Test_To_Time_e953f5_b6f0f8(Gnattest_T: in out Test) renames
     Test_To_Time_test_to_date;
--  id:2.2/e953f5f4ae1398fb/To_Time/1/0/test_to_date/
   procedure Test_To_Time_test_to_date(Gnattest_T: in out Test) is
      function To_Time(Date: String) return Time renames
        Wrap_Test_To_Time_e953f5_b6f0f8;
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
   function Wrap_Test_To_HTTP_Date_2bd5b1_0b38f7(Date: Time) return String is
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
         Test_To_HTTP_Date_2bd5b1_0b38f7_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.AtomFeed.To_HTTP_Date(Date);
      begin
         begin
            pragma Assert(Test_To_HTTP_Date_2bd5b1_0b38f7_Result'Length > 0);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(atomfeed.ads:0:):Test_To_HTTP_Date test commitment violated");
         end;
         return Test_To_HTTP_Date_2bd5b1_0b38f7_Result;
      end;
   end Wrap_Test_To_HTTP_Date_2bd5b1_0b38f7;
--  end read only

--  begin read only
   procedure Test_To_HTTP_Date_test_to_http_date(Gnattest_T: in out Test);
   procedure Test_To_HTTP_Date_2bd5b1_0b38f7(Gnattest_T: in out Test) renames
     Test_To_HTTP_Date_test_to_http_date;
--  id:2.2/2bd5b10ba8625102/To_HTTP_Date/1/0/test_to_http_date/
   procedure Test_To_HTTP_Date_test_to_http_date(Gnattest_T: in out Test) is
      function To_HTTP_Date(Date: Time) return String renames
        Wrap_Test_To_HTTP_Date_2bd5b1_0b38f7;
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
   procedure Wrap_Test_Start_Atom_Feed_2493ea_0ccbf2 is
   begin
      GNATtest_Generated.GNATtest_Standard.AtomFeed.Start_Atom_Feed;
   end Wrap_Test_Start_Atom_Feed_2493ea_0ccbf2;
--  end read only

--  begin read only
   procedure Test_Start_Atom_Feed_test_start_atom_feed
     (Gnattest_T: in out Test);
   procedure Test_Start_Atom_Feed_2493ea_0ccbf2
     (Gnattest_T: in out Test) renames
     Test_Start_Atom_Feed_test_start_atom_feed;
--  id:2.2/2493ea2ede4511ae/Start_Atom_Feed/1/0/test_start_atom_feed/
   procedure Test_Start_Atom_Feed_test_start_atom_feed
     (Gnattest_T: in out Test) is
      procedure Start_Atom_Feed renames
        Wrap_Test_Start_Atom_Feed_2493ea_0ccbf2;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Yass_Config.Atom_Feed_Source := To_Unbounded_String(Source => "tags");
      Start_Atom_Feed;
      Assert
        (Site_Tags("AtomLink") /= "",
         "Failed to start atom feed for the project");

--  begin read only
   end Test_Start_Atom_Feed_test_start_atom_feed;
--  end read only

--  begin read only
   procedure Wrap_Test_Add_Page_To_Feed_467d32_84581a
     (File_Name: String; Entries: in out FeedEntry_Container.Vector) is
   begin
      begin
         pragma Assert(File_Name'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(atomfeed.ads:0):Test_Add_Page_To_Feed test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.AtomFeed.Add_Page_To_Feed
        (File_Name, Entries);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(atomfeed.ads:0:):Test_Add_Page_To_Feed test commitment violated");
      end;
   end Wrap_Test_Add_Page_To_Feed_467d32_84581a;
--  end read only

--  begin read only
   procedure Test_Add_Page_To_Feed_test_add_page_to_feed
     (Gnattest_T: in out Test);
   procedure Test_Add_Page_To_Feed_467d32_84581a
     (Gnattest_T: in out Test) renames
     Test_Add_Page_To_Feed_test_add_page_to_feed;
--  id:2.2/467d32f242a7dd76/Add_Page_To_Feed/1/0/test_add_page_to_feed/
   procedure Test_Add_Page_To_Feed_test_add_page_to_feed
     (Gnattest_T: in out Test) is
      procedure Add_Page_To_Feed
        (File_Name: String; Entries: in out FeedEntry_Container.Vector) renames
        Wrap_Test_Add_Page_To_Feed_467d32_84581a;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Feeds: FeedEntry_Container.Vector;

   begin

      Feeds.Append
        ((Id => To_Unbounded_String("myid"),
          Entry_Title => To_Unbounded_String("my entry"),
          Updated => Ada.Calendar.Time_Of(2_021, 1, 1), others => <>));
      Add_Page_To_Feed
        (To_String(Yass_Config.Output_Directory) & "/test.html", Feeds);
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Add_Page_To_Feed_test_add_page_to_feed;
--  end read only

--  begin read only
   procedure Wrap_Test_Save_Atom_Feed_73b303_2e8e31 is
   begin
      GNATtest_Generated.GNATtest_Standard.AtomFeed.Save_Atom_Feed;
   end Wrap_Test_Save_Atom_Feed_73b303_2e8e31;
--  end read only

--  begin read only
   procedure Test_Save_Atom_Feed_test_save_atom_feed(Gnattest_T: in out Test);
   procedure Test_Save_Atom_Feed_73b303_2e8e31(Gnattest_T: in out Test) renames
     Test_Save_Atom_Feed_test_save_atom_feed;
--  id:2.2/73b303330fe0df08/Save_Atom_Feed/1/0/test_save_atom_feed/
   procedure Test_Save_Atom_Feed_test_save_atom_feed
     (Gnattest_T: in out Test) is
      procedure Save_Atom_Feed renames Wrap_Test_Save_Atom_Feed_73b303_2e8e31;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Create_Path(To_String(Yass_Config.Output_Directory));
      Save_Atom_Feed;
      Assert
        (Exists
           (To_String(Yass_Config.Output_Directory) & Dir_Separator &
            "atom.xml"),
         "Failed to save the project Atom feed to file.");

--  begin read only
   end Test_Save_Atom_Feed_test_save_atom_feed;
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
