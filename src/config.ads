--    Copyright 2019-2021 Bartek thindil Jasicki & 2022-2024 A.J. Ianozi
--
--    This file is part of YASS.
--
--    YASS is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    YASS is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with YASS.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash;
with AWS.Templates; use AWS.Templates;

-- ****h* Yass/Config
-- FUNCTION
-- Provide code for manipulate config file
-- SOURCE
package Config is
-- ****

   -- ****t* Config/Config.Excluded_Container
   -- FUNCTION
   -- Used to store list of excluded files
   -- SOURCE
   package Excluded_Container is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => String);
   -- ****

   -- ****t* Config/Config.Parser_Config
   -- FUNCTION
   -- Data structure for setting for parser
   -- PARAMETERS
   -- Output_Directory        - Path to directory with generated site
   -- Layouts_Directory       - Path to directory where site layouts are
   -- Modules_Directory       - Path to directory where program modules for this
   --                           site are
   -- Excluded_Files          - List of excluded files (and directories) from
   --                           site
   -- Server_Enabled          - Did web server is enabled
   -- Server_Port             - Port on which web server listen
   -- Monitor_Interval        - Interval (in seconds) how often program should
   --                           check for changes in site to regenerate it
   -- Base_Url                - Base URL for site, needed mostly for creating
   --                           sitemap
   -- Sitemap_Enabled         - Did creating sitemap is enabled
   -- Atom_Feed_Source        - Source of atom feed entries. Possible values
   --                           are: none (don't create atom feed, default),
   --                           tags (create entries from tags in markdown files)
   --                           and [filename] (path to markdown file which will
   --                           be used as a source of feed)
   -- Site_Name               - Name of the site, needed for atom feed
   -- Atom_Feed_Amount        - Number of entries in the Atom feed of the site
   -- Markdown_Comment        - String used to mark comments in markdown files
   -- Stop_Server_On_Error    - Did sever should go down if encounter error
   --                           during creating site
   -- Browser_Command         - Command used to open web browser (if empty,
   --                           don't open anything)
   -- Monitor_Config_Interval - Interval (in seconds) how often program should
   --                           check for changes in site configuration to
   --                           reconfigure it
   -- Author_Name             - Name of author of the site, needed for atom feed
   -- Author_Email            - Email address of author of the site, needed for
   --                           atom feed
   -- Language                - ISO code of the language of the site
   -- SOURCE
   type Parser_Config is record
      Output_Directory: Unbounded_String :=
        To_Unbounded_String(Source => "_output");
      Layouts_Directory: Unbounded_String :=
        To_Unbounded_String(Source => "_layouts");
      Modules_Directory: Unbounded_String :=
        To_Unbounded_String(Source => "_modules");
      Excluded_Files: Excluded_Container.Vector;
      Server_Enabled: Boolean := True;
      Server_Port: Positive := 8_888;
      Monitor_Interval: Duration := 5.0;
      Base_Url: Unbounded_String :=
        To_Unbounded_String(Source => "http://localhost:8888");
      Sitemap_Enabled: Boolean := True;
      HTML_Enabled: Boolean := True;
      Atom_Feed_Source: Unbounded_String :=
        To_Unbounded_String(Source => "none");
      Site_Name: Unbounded_String := To_Unbounded_String(Source => "New Site");
      Atom_Feed_Amount: Positive := 25;
      Markdown_Comment: Unbounded_String :=
        To_Unbounded_String(Source => "--");
      Stop_Server_On_Error: Boolean := False;
      Browser_Command: Unbounded_String :=
        To_Unbounded_String(Source => "none");
      Monitor_Config_Interval: Duration := 60.0;
      Author_Name: Unbounded_String :=
        To_Unbounded_String(Source => "John Doe");
      Author_Email: Unbounded_String :=
        To_Unbounded_String(Source => "johndoe@example.com");
      Language: Unbounded_String := To_Unbounded_String(Source => "en");
   end record;
   -- ****

   -- ****d* Config/Config.Default_Parser_Configuration
   -- FUNCTION
   -- Default parser configuration values
   -- SOURCE
   Default_Parser_Configuration: constant Parser_Config := (others => <>);
   -- ****

   --## rule off GLOBAL_REFERENCES
   -- ****v* Config/Config.Yass_Config
   -- FUNCTION
   -- Settings for the program
   -- SOURCE
   Yass_Config: Parser_Config := Default_Parser_Configuration;
   -- ****

   -- ****t* Config/Config.Tags_Container
   -- FUNCTION
   -- Used to store AWS template tags
   -- SOURCE
   package Tags_Container is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String, Element_Type => String, Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   -- ****

   -- ****v* Config/Config.Site_Tags
   -- FUNCTION
   -- Site tags (like title, author, etc)
   -- SOURCE
   Site_Tags: Tags_Container.Map;
   -- ****

   -- ****v* Config/Config.Site_Directory
   -- FUNCTION
   -- Directory where site files are
   -- SOURCE
   Site_Directory: Unbounded_String;
   -- ****

   -- ****t* Config/Config.TableTags_Container
   -- FUNCTION
   -- Used to store AWS template table tags
   -- SOURCE
   package TableTags_Container is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String, Element_Type => Vector_Tag, Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   -- ****

   -- ****v* Config/Config.Global_Table_Tags
   -- FUNCTION
   -- Global table tags, used in @@TABLE@@ statement
   -- SOURCE
   Global_Table_Tags: TableTags_Container.Map;
   -- ****
   --## rule on GLOBAL_REFERENCES

   -- ****e* Config/Config.Invalid_Config_Data
   -- FUNCTION
   -- Raised when invalid data found in site config file
   -- SOURCE
   Invalid_Config_Data: exception;
   -- ****

   -- ****f* Config/Config.Create_Config
   -- FUNCTION
   -- Create default config in directory with full path Directory_Name
   -- PARAMETERS
   -- Directory_Name - Full path to the directory where config file will be
   --                  created
   -- SOURCE
   procedure Create_Config(Directory_Name: String) with
      Pre => Directory_Name'Length > 0,
      Test_Case => (Name => "Test_Create_Config", Mode => Nominal);
   -- ****

   -- ****f* Config/Config.Parse_Config
   -- FUNCTION
   -- Parse config file and set all settings and tags for site in directory
   -- with full path Directory_Name
   -- PARAMETERS
   -- Directory_Name - Full path to the directory from which config file will
   --                  be parsed
   -- SOURCE
   procedure Parse_Config(Directory_Name: String) with
      Pre => Directory_Name'Length > 0,
      Test_Case => (Name => "Test_Parse_Config", Mode => Nominal);
   -- ****

   -- ****f* Config/Config.Create_Interactive_Config
   -- FUNCTION
   -- Create configuration file based on the user answers to the program
   -- questions
   -- PARAMETERS
   -- Directory_Name - Full path to the directory where config file will be
   --                  created
   -- SOURCE
   procedure Create_Interactive_Config(Directory_Name: String) with
      Pre => Directory_Name'Length > 0;
   -- ****

end Config;
