--    Copyright 2019-2021 Bartek thindil Jasicki
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.UTF_Encoding.Strings; use Ada.Strings.UTF_Encoding.Strings;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.String_Split; use GNAT.String_Split;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Config is

   procedure Create_Config(Directory_Name: String) is
      Config_File: File_Type;
   begin
      Create
        (File => Config_File, Mode => Append_File,
         Name => Directory_Name & Dir_Separator & "site.cfg");
      Put_Line
        (File => Config_File,
         Item =>
           "# Directory in which will be placed HTML files with site layout (templates). May be absolute or relative to project directory.");
      Put_Line(File => Config_File, Item => "LayoutsDirectory = _layouts");
      Put_Line
        (File => Config_File,
         Item =>
           "# Directory in which will be placed generated site. May be absolute or relative to project directory.");
      Put_Line(File => Config_File, Item => "OutputDirectory = _output");
      Put_Line
        (File => Config_File,
         Item =>
           "# Directory in which will be placed program modules used to generate the site. May be absolute or relative to project directory.");
      Put_Line(File => Config_File, Item => "ModulesDirectory = _modules");
      Put_Line
        (File => Config_File,
         Item =>
           "# List of excluded files and directories from list of sources used to generating the site. All paths must be relative to the project directory. If you exclude directory, it whole content will be excluded too. Layouts, modules and output directories are excluded by default.");
      Put_Line
        (File => Config_File, Item => "ExcludedFiles = .git,.gitignore,tags");
      Put_Line
        (File => Config_File,
         Item =>
           "# The name of the site which will be created. If you have enabled creating Atom feed then it is needed. Otherwise, you can use it as a normal template tag.");
      Put_Line(File => Config_File, Item => "Name = New Site");
      Put_Line
        (File => Config_File,
         Item =>
           "# The description of the site which will be created. Must be in one line, no new line allowed. It is used to set meta tag description (which is showed in search engines results) but only when pages don't set it. Optional setting.");
      Put_Line(File => Config_File, Item => "Description = My new site");
      Put_Line
        (File => Config_File,
         Item =>
           "# The ISO 639-1 language code in which the site will be created.");
      Put_Line(File => Config_File, Item => "Language = en");
      Put_Line
        (File => Config_File,
         Item =>
           "# Name of author of the site. If you have enable creating Atom feed, then it is needed. Otherwise, you can use it as a normal template tag. It is also used in setting meta tag author for all pages.");
      Put_Line(File => Config_File, Item => "Author = John Doe");
      Put_Line
        (File => Config_File,
         Item =>
           "# Email address of author of the site. If you have enable creating Atom feed, then it is needed. Otherwise, you can use it as a normal template tag.");
      Put_Line
        (File => Config_File, Item => "AuthorEmail = johndoe@example.com");
      Put_Line
        (File => Config_File,
         Item =>
           "# Base URL of the site. It is needed mostly for creating sitemap and Atom feed, but you can use it as a normal the site tag. If your site will be available at https://mysite.com/blog then this will be your BaseURL.");
      Put_Line(File => Config_File, Item => "BaseURL = http://localhost:8888");
      Put_Line
        (File => Config_File,
         Item =>
           "# Source which will be used for creating Atom feed of the site. Possible values are: none: don't create atom feed, tags: create Atom entries from proper tags in .md files, [filename]: the path (related to the project directory path) to markdown file which will be used as a source of atom feed (must have proper tags set inside).");
      Put_Line(File => Config_File, Item => "AtomFeedSource = none");
      Put_Line
        (File => Config_File,
         Item =>
           "# Number of entries in the Atom feed of the site. Try not set it too high, recommended values are between 10 and 50.");
      Put_Line(File => Config_File, Item => "AtomFeedAmount = 25");
      Put_Line
        (File => Config_File,
         Item =>
           "# Should the program create sitemap when creating the site. Possible values are true or false (case-insensitive).");
      Put_Line(File => Config_File, Item => "SitemapEnabled = true");
      Put_Line
        (File => Config_File,
         Item =>
           "# Should the program start web server when monitoring for changes in site. Possible values are true or false (case-insensitive).");
      Put_Line(File => Config_File, Item => "ServerEnabled = true");
      Put_Line
        (File => Config_File,
         Item =>
           "# Port on which web server will be listen if enabled. Possible values are from 1 to 65535. Please remember, that ports below 1025 require root privileges to work.");
      Put_Line(File => Config_File, Item => "ServerPort = 8888");
      Put_Line
        (File => Config_File,
         Item =>
           "# Should web server and whole monitoring of the site changes stop if encounter any error during the site creation.  Possible values are true or false (case-insensitive).");
      Put_Line(File => Config_File, Item => "StopServerOnError = false");
      Put_Line
        (File => Config_File,
         Item =>
           "# Full path to the command which will be used to start the web browser with index.html page of the site. String ""%s"" (without quotes) will be replaced by server URL. If this setting is ""none"", the web browser will be not started, same as when the web server is disabled.");
      Put_Line(File => Config_File, Item => "BrowserCommand = none");
      Put_Line
        (File => Config_File,
         Item =>
           "# How often (in seconds) the program should monitor site for changes and regenerate it if needed. Can be any positive number, but you probably don't want to set it to check every few thousands years :)");
      Put_Line(File => Config_File, Item => "MonitorInterval = 5");
      Put_Line
        (File => Config_File,
         Item =>
           "# How often (in seconds) the program should monitor site configuration for changes and reconfigure it if needed. Can be any positive number.");
      Put_Line(File => Config_File, Item => "MonitorConfigInterval = 60");
      Put_Line
        (File => Config_File,
         Item =>
           "# String used to mark start of the templates tags, used in templates files. You may want to change it, if you want to use templates from other static site generator.");
      Put_Line(File => Config_File, Item => "Start_TagSeparator = {%");
      Put_Line
        (File => Config_File,
         Item =>
           "# String used to mark end of the templates tags, used in templates files. You may want to change it, if you want to use templates from other static site generator.");
      Put_Line(File => Config_File, Item => "EndTagSeparator = %}");
      Put_Line
        (File => Config_File,
         Item =>
           "# String used to mark comments in markdown files which will be parsed.");
      Put_Line(File => Config_File, Item => "MarkdownComment = --");
      Put_Line
        (File => Config_File,
         Item =>
           "# Site tags, optional. Tags can be 4 types: strings, boolean, numeric or composite.");
      Put_Line
        (File => Config_File,
         Item =>
           "# First 3 types of tags are in Name = Value scheme. For strings, it can be any alphanumeric value without new line sign. For boolean it must be ""true"" or ""false"", for numeric any number. Program will detect self which type of tag is and properly set it. It always falls back to string value.");
      Put_Line
        (File => Config_File,
         Item =>
           "# Composite tags first must be initialized with Name = [] then just add as many as you want values to it by Name = Value scheme.");
      Put_Line
        (File => Config_File,
         Item =>
           "# For more information about site.cfg file please check program documentation.");
      Close(File => Config_File);
   end Create_Config;

   procedure Parse_Config(Directory_Name: String) is
      Config_File: File_Type;
      Raw_Data, Field_Name, Value: Unbounded_String := Null_Unbounded_String;
      Equal_Index: Natural := 0;
      Tokens: Slice_Set; --## rule line off IMPROPER_INITIALIZATION
      Start_Tag: Unbounded_String := To_Unbounded_String(Source => "{%");
      End_Tag: Unbounded_String := To_Unbounded_String(Source => "%}");
      procedure Normalize_Dir(Directory_Path: in out Unbounded_String) is
      begin
         if Dir_Separator = '/'
           and then Element(Source => Directory_Path, Index => 1) /= '/' then
            Directory_Path :=
              To_Unbounded_String(Source => Directory_Name & Dir_Separator) &
              Directory_Path;
         elsif Element(Source => Directory_Path, Index => 2) /= ':' then
            Directory_Path :=
              To_Unbounded_String(Source => Directory_Name & Dir_Separator) &
              Directory_Path;
         end if;
      end Normalize_Dir;
   begin
      Site_Tags.Clear;
      Global_Table_Tags.Clear;
      Open
        (File => Config_File, Mode => In_File,
         Name => Directory_Name & "/site.cfg");
      Load_Configuration_Loop :
      while not End_Of_File(File => Config_File) loop
         Raw_Data :=
           To_Unbounded_String
             (Source => Encode(Item => Get_Line(File => Config_File)));
         if Length(Source => Raw_Data) = 0
           or else Element(Source => Raw_Data, Index => 1) = '#' then
            goto End_Of_Loop;
         end if;
         Equal_Index := Index(Source => Raw_Data, Pattern => "=");
         if Equal_Index = 0 then
            raise Invalid_Config_Data with To_String(Source => Raw_Data);
         end if;
         Field_Name := Head(Source => Raw_Data, Count => Equal_Index - 2);
         Value :=
           Tail
             (Source => Raw_Data,
              Count => Length(Source => Raw_Data) - Equal_Index - 1);
         if Field_Name = To_Unbounded_String(Source => "LayoutsDirectory") then
            Yass_Config.Layouts_Directory := Value;
         elsif Field_Name =
           To_Unbounded_String(Source => "OutputDirectory") then
            Yass_Config.Output_Directory := Value;
         elsif Field_Name =
           To_Unbounded_String(Source => "ModulesDirectory") then
            Yass_Config.Modules_Directory := Value;
         elsif Field_Name = To_Unbounded_String(Source => "ExcludedFiles") then
            Create
              (S => Tokens, From => To_String(Source => Value),
               Separators => ",");
            Add_Excluded_Files_Loop :
            for I in 1 .. Slice_Count(S => Tokens) loop
               Yass_Config.Excluded_Files.Append
                 (New_Item => Slice(S => Tokens, Index => I));
            end loop Add_Excluded_Files_Loop;
         elsif Field_Name = To_Unbounded_String(Source => "ServerEnabled") then
            Yass_Config.Server_Enabled :=
              (if To_Lower(Item => To_String(Source => Value)) = "true" then
                 True
               else False);
         elsif Field_Name = To_Unbounded_String(Source => "ServerPort") then
            Yass_Config.Server_Port :=
              Positive'Value(To_String(Source => Value));
            if Yass_Config.Server_Port > 65_535 then
               raise Invalid_Config_Data with To_String(Source => Raw_Data);
            end if;
         elsif Field_Name =
           To_Unbounded_String(Source => "StopServerOnError") then
            Yass_Config.Stop_Server_On_Error :=
              (if To_Lower(Item => To_String(Source => Value)) = "true" then
                 True
               else False);
         elsif Field_Name =
           To_Unbounded_String(Source => "BrowserCommand") then
            if Index(Source => Value, Pattern => "%s", From => 1) > 0 then
               Replace_Slice
                 (Source => Value,
                  Low => Index(Source => Value, Pattern => "%s", From => 1),
                  High =>
                    Index(Source => Value, Pattern => "%s", From => 1) + 1,
                  By =>
                    "http://localhost:" &
                    Trim
                      (Source => Positive'Image(Yass_Config.Server_Port),
                       Side => Ada.Strings.Left));
            end if;
            Yass_Config.Browser_Command := Value;
         elsif Field_Name =
           To_Unbounded_String(Source => "MonitorInterval") then
            Yass_Config.Monitor_Interval :=
              Duration'Value(To_String(Source => Value));
         elsif Field_Name =
           To_Unbounded_String(Source => "MonitorConfigInterval") then
            Yass_Config.Monitor_Config_Interval :=
              Duration'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "BaseURL") then
            Yass_Config.Base_Url := Value;
            Site_Tags.Include
              (Key => "BaseURL", New_Item => To_String(Source => Value));
         elsif Field_Name =
           To_Unbounded_String(Source => "SitemapEnabled") then
            Yass_Config.Sitemap_Enabled :=
              (if To_Lower(Item => To_String(Source => Value)) = "true" then
                 True
               else False);
         elsif Field_Name =
           To_Unbounded_String(Source => "AtomFeedSource") then
            if Value in To_Unbounded_String(Source => "none") |
                  To_Unbounded_String(Source => "tags") then
               Yass_Config.Atom_Feed_Source := Value;
            else
               Yass_Config.Atom_Feed_Source :=
                 Unbounded_Slice
                   (Source => Value, Low => 1,
                    High => Length(Source => Value) - 2) &
                 To_Unbounded_String(Source => "html");
            end if;
         elsif Field_Name =
           To_Unbounded_String(Source => "AtomFeedAmount") then
            Yass_Config.Atom_Feed_Amount :=
              Positive'Value(To_String(Source => Value));
         elsif Field_Name = To_Unbounded_String(Source => "Name") then
            Yass_Config.Site_Name := Value;
            Site_Tags.Include
              (Key => To_String(Source => Field_Name),
               New_Item => To_String(Source => Value));
         elsif Field_Name =
           To_Unbounded_String(Source => "Start_TagSeparator") then
            Start_Tag := Value;
         elsif Field_Name =
           To_Unbounded_String(Source => "EndTagSeparator") then
            End_Tag := Value;
         elsif Field_Name =
           To_Unbounded_String(Source => "MarkdownComment") then
            Yass_Config.Markdown_Comment := Value;
         elsif Field_Name = To_Unbounded_String(Source => "Author") then
            Yass_Config.Author_Name := Value;
         elsif Field_Name = To_Unbounded_String(Source => "AuthorEmail") then
            Yass_Config.Author_Email := Value;
         elsif Field_Name = To_Unbounded_String(Source => "Language") then
            Yass_Config.Language := Value;
            Site_Tags.Include
              (Key => To_String(Source => Field_Name),
               New_Item => To_String(Source => Value));
         elsif Value = To_Unbounded_String(Source => "[]") then
            Global_Table_Tags.Include
              (Key => To_String(Source => Field_Name), New_Item => +"");
            Clear(T => Global_Table_Tags(To_String(Source => Field_Name)));
         elsif TableTags_Container.Contains
             (Global_Table_Tags, To_String(Field_Name)) then
            Global_Table_Tags(To_String(Field_Name)) :=
              Global_Table_Tags(To_String(Field_Name)) & Value;
         else
            Tags_Container.Include
              (Site_Tags, To_String(Field_Name), To_String(Value));
         end if;
         <<End_Of_Loop>>
      end loop Load_Configuration_Loop;
      Close(Config_File);
      Normalize_Dir(Yass_Config.Layouts_Directory);
      Normalize_Dir(Yass_Config.Output_Directory);
      Normalize_Dir(Yass_Config.Modules_Directory);
      Yass_Config.Excluded_Files.Append(".");
      Yass_Config.Excluded_Files.Append("..");
      Yass_Config.Excluded_Files.Append("site.cfg");
      Yass_Config.Excluded_Files.Append
        (Simple_Name(To_String(Yass_Config.Layouts_Directory)));
      Yass_Config.Excluded_Files.Append
        (Simple_Name(To_String(Yass_Config.Output_Directory)));
      Yass_Config.Excluded_Files.Append
        (Simple_Name(To_String(Yass_Config.Modules_Directory)));
      Site_Directory := To_Unbounded_String(Directory_Name);
      Set_Tag_Separators(To_String(Start_Tag), To_String(End_Tag));
   exception
      when others =>
         raise Invalid_Config_Data with To_String(Raw_Data);
   end Parse_Config;

   procedure Create_Interactive_Config(Directory_Name: String) is
      Config_File: File_Type;
      Answer: Unbounded_String;
   begin
      Create
        (Config_File, Append_File,
         Directory_Name & Dir_Separator & "site.cfg");
      Put_Line
        (Config_File,
         "# Directory in which will be placed HTML files with site layout (templates). May be absolute or relative to project directory.");
      Put_Line(Config_File, "LayoutsDirectory = _layouts");
      Put_Line
        (Config_File,
         "# Directory in which will be placed generated site. May be absolute or relative to project directory.");
      Put_Line(Config_File, "OutputDirectory = _output");
      Put_Line
        (Config_File,
         "# Directory in which will be placed program modules used to generate the site. May be absolute or relative to project directory.");
      Put_Line(Config_File, "ModulesDirectory = _modules");
      Put_Line
        (Config_File,
         "# List of excluded files and directories from list of sources used to generating the site. All paths must be relative to the project directory. If you exclude directory, it whole content will be excluded too. Layouts, modules and output directories are excluded by default.");
      Put_Line(Config_File, "ExcludedFiles = .git,.gitignore,tags");
      Put_Line
        (Config_File,
         "# The name of the site which will be created. If you have enabled creating Atom feed then it is needed. Otherwise, you can use it as a normal template tag.");
      Put_Line
        ("Now we ask you some questions about your new site. You can always change it later by modifying the site configuration file. If you just press Enter as a answer, default value will be used.");
      Put("Please enter the name of the new site (default - New Site): ");
      Answer := To_Unbounded_String(Get_Line);
      if Answer = Null_Unbounded_String then
         Answer := To_Unbounded_String("New Site");
      end if;
      Put_Line(Config_File, "Name = " & To_String(Answer));
      Put_Line
        (Config_File,
         "# The description of the site which will be created. Must be in one line, no new line allowed. It is used to set meta tag description (which is showed in search engines results) but only when pages don't set it. Optional setting.");
      Put_Line
        ("Please enter the description of the new site (default - My new site). It is used to create meta tag for the website (which is showed in search engines results) but only if pages don't set own. Must be set in one line, no new line allowed.");
      Answer := To_Unbounded_String(Get_Line);
      if Answer /= Null_Unbounded_String then
         Answer := To_Unbounded_String("My new site");
      end if;
      Put_Line(Config_File, "Description = " & To_String(Answer));
      Put_Line
        (Config_File,
         "# The ISO 639-1 language code in which the site will be created.");
      Put
        ("Please enter language code in which the new site will be written (default - en): ");
      Answer := To_Unbounded_String(Get_Line);
      if Answer = Null_Unbounded_String then
         Answer := To_Unbounded_String("en");
      end if;
      Put_Line(Config_File, "Language = " & To_String(Answer));
      Put_Line
        (Config_File,
         "# Name of author of the site. If you have enable creating Atom feed, then it is needed. Otherwise, you can use it as a normal template tag. It is also used to set meta tag author for all pages.");
      Put("Please enter the author of the new site (default - Jon Doe): ");
      Answer := To_Unbounded_String(Get_Line);
      if Answer = Null_Unbounded_String then
         Answer := To_Unbounded_String("Jon Doe");
      end if;
      Put_Line(Config_File, "Author = " & To_String(Answer));
      Put_Line
        (Config_File,
         "# Email address of author of the site. If you have enable creating Atom feed, then it is needed. Otherwise, you can use it as a normal template tag.");
      Put
        ("Please enter the contact email for the new site (default - jondoe@example.com): ");
      Answer := To_Unbounded_String(Get_Line);
      if Answer = Null_Unbounded_String then
         Answer := To_Unbounded_String("jondoe@example.com");
      end if;
      Put_Line(Config_File, "AuthorEmail = " & To_String(Answer));
      Put_Line
        (Config_File,
         "# Base URL of the site. It is needed mostly for creating sitemap and Atom feed, but you can use it as a normal the site tag. If your site will be available at https://mysite.com/blog then this will be your BaseURL.");
      Put
        ("Please enter base URL of the new site (default - http://localhost:8888): ");
      Answer := To_Unbounded_String(Get_Line);
      if Answer = Null_Unbounded_String then
         Answer := To_Unbounded_String("http://localhost:8888");
      end if;
      Put_Line(Config_File, "BaseURL = " & To_String(Answer));
      Put_Line
        (Config_File,
         "# Source which will be used for creating Atom feed of the site. Possible values are: none - don't create atom feed, tags - create Atom entries from proper tags in .md files, [filename] - the path (related to the project directory path) to markdown file which will be used as a source of atom feed (must have proper tags set inside).");
      Put
        ("Do you want to create Atom feed for the new site? If yes, you must specify source for the feed: tags - create Atom entries from proper tags in Markdown files, filename - the path (related to the project directory path) to markdown file which will be used as a source of atom feed (must have proper tags set inside). If you press Enter, creating Atom feed will be disabled (default - none): ");
      Answer := To_Unbounded_String(Get_Line);
      if Answer = Null_Unbounded_String then
         Answer := To_Unbounded_String("none");
      end if;
      Put_Line(Config_File, "AtomFeedSource = " & To_String(Answer));
      if Answer /= To_Unbounded_String("none") then
         Put
           ("How much maximum entries should be in the Atom feed? Recommended valuese are between 10 and 50 (default - 25): ");
         Answer := To_Unbounded_String(Get_Line);
         if Answer = Null_Unbounded_String then
            Answer := To_Unbounded_String("25");
         end if;
      else
         Answer := To_Unbounded_String("25");
      end if;
      Put_Line
        (Config_File,
         "# Number of entries in the Atom feed of the site. Try not set it too high, recommended values are between 10 and 50.");
      Put_Line(Config_File, "AtomFeedAmount = " & To_String(Answer));
      Put_Line
        (Config_File,
         "# Should the program create sitemap when creating the site. Possible values are true or false (case-insensitive).");
      Put
        ("Do you want to create sitemap file for the new site? (default - yes): ");
      Answer := To_Unbounded_String(Get_Line);
      if Answer = To_Unbounded_String("yes") or
        Answer = To_Unbounded_String("y") or
        Answer = Null_Unbounded_String then
         Put_Line(Config_File, "SitemapEnabled = true");
      else
         Put_Line(Config_File, "SitemapEnabled = false");
      end if;
      Put
        ("Do you want to set more technical options (like configuring build-in web server)? (default - no): ");
      Answer := To_Unbounded_String(Get_Line);
      if Answer = To_Unbounded_String("yes") or
        Answer = To_Unbounded_String("y") then
         Put_Line
           (Config_File,
            "# Should the program start web server when monitoring for changes in site. Possible values are true or false (case-insensitive).");
         Put
           ("Should the program start web server when monitoring for changes in the site? (default - yes): ");
         Answer := To_Unbounded_String(Get_Line);
         if Answer = To_Unbounded_String("yes") or
           Answer = To_Unbounded_String("y") or
           Answer = Null_Unbounded_String then
            Put_Line(Config_File, "ServerEnabled = true");
         else
            Put_Line(Config_File, "ServerEnabled = false");
         end if;
         Put_Line(Config_File, "ServerEnabled = " & To_String(Answer));
         Put_Line
           (Config_File,
            "# Port on which web server will be listen if enabled. Possible values are from 1 to 65535. Please remember, that ports below 1025 require root privileges to work.");
         Put
           ("On which port should the web server listening? Possible values are from 1 to 65535. Ports below 1025 require root privileges. (default - 8888): ");
         Answer := To_Unbounded_String(Get_Line);
         if Answer = Null_Unbounded_String then
            Answer := To_Unbounded_String("8888");
         end if;
         Put_Line(Config_File, "ServerPort = " & To_String(Answer));
         Put_Line
           (Config_File,
            "# Should web server and whole monitoring of the site changes stop if encounter any error during the site creation.  Possible values are true or false (case-insensitive).");
         Put
           ("Should whole monitoring option stop if encounter any error during the site creation? (default - no): ");
         Answer := To_Unbounded_String(Get_Line);
         if Answer = To_Unbounded_String("yes") or
           Answer = To_Unbounded_String("y") then
            Put_Line(Config_File, "StopServerOnError = true");
         else
            Put_Line(Config_File, "StopServerOnError = false");
         end if;
         Put_Line
           (Config_File,
            "# Full path to the command which will be used to start the web browser with index.html page of the site. String ""%s"" (without quotes) will be replaced by server URL. If this setting is ""none"", the web browser will be not started, same as when the web server is disabled.");
         Put
           ("Full path to the web broser which will be started when the program starts in server mode. (default - none): ");
         Answer := To_Unbounded_String(Get_Line);
         if Answer = Null_Unbounded_String then
            Answer := To_Unbounded_String("none");
         end if;
         Put_Line(Config_File, "BrowserCommand = " & To_String(Answer));
         Put_Line
           (Config_File,
            "# How often (in seconds) the program should monitor site for changes and regenerate it if needed. Can be any positive number, but you probably don't want to set it to check every few thousands years :)");
         Put
           ("How often, in seconds, the program should check for changes in the site files? (default - 5): ");
         Answer := To_Unbounded_String(Get_Line);
         if Answer = Null_Unbounded_String then
            Answer := To_Unbounded_String("5");
         end if;
         Put_Line(Config_File, "MonitorInterval = " & To_String(Answer));
         Put_Line
           (Config_File,
            "# How often (in seconds) the program should monitor site configuration for changes and reconfigure it if needed. Can be any positive number.");
         Put
           ("How often, in seconds, the program should check for changes in the site configuration file? (default - 60): ");
         Answer := To_Unbounded_String(Get_Line);
         if Answer = Null_Unbounded_String then
            Answer := To_Unbounded_String("60");
         end if;
         Put_Line(Config_File, "MonitorConfigInterval = " & To_String(Answer));
      else
         Put_Line
           (Config_File,
            "# Should the program start web server when monitoring for changes in site. Possible values are true or false (case-insensitive).");
         Put_Line(Config_File, "ServerEnabled = true");
         Put_Line
           (Config_File,
            "# Port on which web server will be listen if enabled. Possible values are from 1 to 65535. Please remember, that ports below 1025 require root privileges to work.");
         Put_Line(Config_File, "ServerPort = 8888");
         Put_Line
           (Config_File,
            "# Should web server and whole monitoring of the site changes stop if encounter any error during the site creation.  Possible values are true or false (case-insensitive).");
         Put_Line(Config_File, "StopServerOnError = false");
         Put_Line
           (Config_File,
            "# Full path to the command which will be used to start the web browser with index.html page of the site. String ""%s"" (without quotes) will be replaced by server URL. If this setting is ""none"", the web browser will be not started, same as when the web server is disabled.");
         Put_Line(Config_File, "BrowserCommand = none");
         Put_Line
           (Config_File,
            "# How often (in seconds) the program should monitor site for changes and regenerate it if needed. Can be any positive number, but you probably don't want to set it to check every few thousands years :)");
         Put_Line(Config_File, "MonitorInterval = 5");
         Put_Line
           (Config_File,
            "# How often (in seconds) the program should monitor site configuration for changes and reconfigure it if needed. Can be any positive number.");
         Put_Line(Config_File, "MonitorConfigInterval = 60");
      end if;
      Put
        ("Do you want to set options related to compatybility with other static sites generators? (default - no): ");
      Answer := To_Unbounded_String(Get_Line);
      if Answer = To_Unbounded_String("yes") or
        Answer = To_Unbounded_String("y") then
         Put_Line
           (Config_File,
            "# String used to mark start of the templates tags, used in templates files. You may want to change it, if you want to use templates from other static site generator.");
         Put
           ("What mark should be used as a start for template tag? (default, without quotes - ""{%""): ");
         Answer := To_Unbounded_String(Get_Line);
         if Answer = Null_Unbounded_String then
            Answer := To_Unbounded_String("{%");
         end if;
         Put_Line(Config_File, "StartTagSeparator = " & To_String(Answer));
         Put_Line
           (Config_File,
            "# String used to mark end of the templates tags, used in templates files. You may want to change it, if you want to use templates from other static site generator.");
         Put
           ("What mark should be used as an end for template tag? (default, without quotes - ""%}""): ");
         Answer := To_Unbounded_String(Get_Line);
         if Answer = Null_Unbounded_String then
            Answer := To_Unbounded_String("%}");
         end if;
         Put_Line(Config_File, "EndTagSeparator = " & To_String(Answer));
         Put_Line
           (Config_File,
            "# String used to mark comments in markdown files which will be parsed.");
         Put
           ("What mark should be used as a start for the comment line in Markdown files? (default, without quotes - ""--""): ");
         Answer := To_Unbounded_String(Get_Line);
         if Answer = Null_Unbounded_String then
            Answer := To_Unbounded_String("--");
         end if;
         Put_Line(Config_File, "MarkdownComment = " & To_String(Answer));
      else
         Put_Line
           (Config_File,
            "# String used to mark start of the templates tags, used in templates files. You may want to change it, if you want to use templates from other static site generator.");
         Put_Line(Config_File, "StartTagSeparator = {%");
         Put_Line
           (Config_File,
            "# String used to mark end of the templates tags, used in templates files. You may want to change it, if you want to use templates from other static site generator.");
         Put_Line(Config_File, "EndTagSeparator = %}");
         Put_Line
           (Config_File,
            "# String used to mark comments in markdown files which will be parsed.");
         Put_Line(Config_File, "MarkdownComment = --");
      end if;
      Put_Line
        (Config_File,
         "# Site tags, optional. Tags can be 4 types: strings, boolean, numeric or composite.");
      Put_Line
        (Config_File,
         "# First 3 types of tags are in Name = Value scheme. For strings it can be any alphanumeric value without new line sign. For boolean it must be ""true"" or ""false"", for numeric any number. Program will detect self which type of tag is and properly set it. It always fall back to string value.");
      Put_Line
        (Config_File,
         "# Composite tags first must be initialized with Name = [] then just add as many as you want values to it by Name = Value scheme.");
      Put_Line
        (Config_File,
         "# For more informations about site.cfg file please check program documentation.");
      Close(Config_File);
   end Create_Interactive_Config;

end Config;
