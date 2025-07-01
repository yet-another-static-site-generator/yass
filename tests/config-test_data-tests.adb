--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Config.Test_Data.

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
package body Config.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Create_Config_683023_a99277(Directory_Name: String) is
   begin
      begin
         pragma Assert(Directory_Name'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(config.ads:0:):Test_Create_Config test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Config.Create_Config
        (Directory_Name);
   end Wrap_Test_Create_Config_683023_a99277;
--  end read only

--  begin read only
   procedure Test_Create_Config_test_create_config(Gnattest_T: in out Test);
   procedure Test_Create_Config_683023_a99277(Gnattest_T: in out Test) renames
     Test_Create_Config_test_create_config;
--  id:2.2/6830236232fc055b/Create_Config/1/0/test_create_config/
   procedure Test_Create_Config_test_create_config(Gnattest_T: in out Test) is
      procedure Create_Config(Directory_Name: String) renames
        Wrap_Test_Create_Config_683023_a99277;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Create_Config(".");
      Assert
        (Exists("site.cfg"),
         "Failed to create the project configuration file.");

--  begin read only
   end Test_Create_Config_test_create_config;
--  end read only

--  begin read only
   procedure Wrap_Test_Parse_Config_31244b_5fe7b0(Directory_Name: String) is
   begin
      begin
         pragma Assert(Directory_Name'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(config.ads:0:):Test_Parse_Config test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Config.Parse_Config(Directory_Name);
   end Wrap_Test_Parse_Config_31244b_5fe7b0;
--  end read only

--  begin read only
   procedure Test_Parse_Config_test_parse_config(Gnattest_T: in out Test);
   procedure Test_Parse_Config_31244b_5fe7b0(Gnattest_T: in out Test) renames
     Test_Parse_Config_test_parse_config;
--  id:2.2/31244ba1905b93dd/Parse_Config/1/0/test_parse_config/
   procedure Test_Parse_Config_test_parse_config(Gnattest_T: in out Test) is
      procedure Parse_Config(Directory_Name: String) renames
        Wrap_Test_Parse_Config_31244b_5fe7b0;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Yass_Config.Language := To_Unbounded_String("pl");
      Parse_Config(".");
      Assert
        (Yass_Config.Language = To_Unbounded_String("en"),
         "Failed to parse the program configuration file.");

--  begin read only
   end Test_Parse_Config_test_parse_config;
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
end Config.Test_Data.Tests;
