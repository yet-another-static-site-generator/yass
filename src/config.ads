--    Copyright 2019 Bartek thindil Jasicki
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

-- ****h* Config/Config
-- FUNCTION
-- Provide code for manipulate config file
-- SOURCE
package Config is
-- ****

   -- ****t* Config/Excluded_Container
   -- FUNCTION
   -- Used to store list of excluded files
   -- SOURCE
   package Excluded_Container is new Ada.Containers.Indefinite_Vectors
     (Positive, String);
   -- ****

   -- ****t* Config/ParserConfig
   -- FUNCTION
   -- Data structure for setting for parser
   -- PARAMETERS
   -- OutputDirectory       - Path to directory with generated site
   -- LayoutsDirectory      - Path to directory where site layouts are
   -- ModulesDirectory      - Path to directory where program modules for this
   --                         site are
   -- ExcludedFiles         - List of excluded files (and directories) from
   --                         site
   -- ServerEnabled         - Did web server is enabled
   -- ServerPort            - Port on which web server listen
   -- MonitorInterval       - Interval (in seconds) how often program should
   --                         check for changes in site to regenerate it
   -- BaseURL               - Base URL for site, needed mostly for creating
   --                         sitemap
   -- SitemapEnabled        - Did creating sitemap is enabled
   -- AtomFeedSource        - Source of atom feed entries. Possible values
   --                         are: none (don't create atom feed, default),
   --                         tags (create entries from tags in markdown files)
   --                         and [filename] (path to markdown file which will
   --                         be used as a source of feed)
   -- SiteName              - Name of the site, needed for atom feed
   -- AtomFeedAmount        - Number of entries in the Atom feed of the site
   -- MarkdownComment       - String used to mark comments in markdown files
   -- StopServerOnError     - Did sever should go down if encounter error
   --                         during creating site
   -- BrowserCommand        - Command used to open web browser (if empty,
   --                         don't open anything)
   -- MonitorConfigInterval - Interval (in seconds) how often program should
   --                         check for changes in site configuration to
   --                         reconfigure it
   -- AuthorName            - Name of author of the site, needed for atom feed
   -- AuthorEmail           - Email address of author of the site, needed for
   --                         atom feed
   -- SOURCE
   type ParserConfig is record
      OutputDirectory: Unbounded_String := To_Unbounded_String("_output");
      LayoutsDirectory: Unbounded_String := To_Unbounded_String("_layouts");
      ModulesDirectory: Unbounded_String := To_Unbounded_String("_modules");
      ExcludedFiles: Excluded_Container.Vector;
      ServerEnabled: Boolean := True;
      ServerPort: Positive := 8888;
      MonitorInterval: Duration := 5.0;
      BaseURL: Unbounded_String :=
        To_Unbounded_String("http://localhost:8888");
      SitemapEnabled: Boolean := True;
      AtomFeedSource: Unbounded_String := To_Unbounded_String("none");
      SiteName: Unbounded_String := To_Unbounded_String("New Site");
      AtomFeedAmount: Positive := 25;
      MarkdownComment: Unbounded_String := To_Unbounded_String("--");
      StopServerOnError: Boolean := False;
      BrowserCommand: Unbounded_String := To_Unbounded_String("none");
      MonitorConfigInterval: Duration := 60.0;
      AuthorName: Unbounded_String := To_Unbounded_String("John Doe");
      AuthorEmail: Unbounded_String :=
        To_Unbounded_String("johndoe@example.com");
   end record;
   -- ****

   -- ****v* Config/YassConfig
   -- FUNCTION
   -- Settings for the program
   -- SOURCE
   YassConfig: ParserConfig;
   -- ****

   -- ****t* Config/Tags_Container
   -- FUNCTION
   -- Used to store AWS template tags
   -- SOURCE
   package Tags_Container is new Ada.Containers.Indefinite_Hashed_Maps(String,
      String, Ada.Strings.Hash, "=");
   -- ****

   -- ****v* Config/SiteTags
   -- FUNCTION
   -- Site tags (like title, author, etc)
   -- SOURCE
   SiteTags: Tags_Container.Map;
   -- ****

   -- ****v* Config/SiteDirectory
   -- FUNCTION
   -- Directory where site files are
   -- SOURCE
   SiteDirectory: Unbounded_String;
   -- ****

   -- ****t* Config/TableTags_Container
   -- FUNCTION
   -- Used to store AWS template table tags
   -- SOURCE
   package TableTags_Container is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Vector_Tag, Ada.Strings.Hash, "=");
   -- ****

   -- ****v* Config/GlobalTableTags
   -- FUNCTION
   -- Global table tags, used in @@TABLE@@ statement
   -- SOURCE
   GlobalTableTags: TableTags_Container.Map;
   -- ****

   -- ****e* Config/InvalidConfigData
   -- FUNCTION
   -- Raised when invalid data found in site config file
   -- SOURCE
   InvalidConfigData: exception;
   -- ****

   -- ****f* Config/CreateConfig
   -- FUNCTION
   -- Create default config in directory with full path DirectoryName
   -- PARAMETERS
   -- DirectoryName - Full path to the directory where config file will be
   --                 created
   -- SOURCE
   procedure CreateConfig(DirectoryName: String);
   -- ****

   -- ****f* Config/ParseConfig
   -- FUNCTION
   -- Parse config file and set all settings and tags for site in directory
   -- with full path DirectoryName
   -- PARAMETERS
   -- DirectoryName - Full path to the directory from which config file will
   --                 be parsed
   -- SOURCE
   procedure ParseConfig(DirectoryName: String);
   -- ****

end Config;
