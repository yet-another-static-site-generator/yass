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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Fixed;
with GNAT.String_Split;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Config is

   ------------------------
   -- Create_Site_Config --
   ------------------------

   procedure Create_Site_Config (Directory_Name : String)
   is
      use Ada.Characters.Handling;

      Config_File : File_Type;

      Image_Site_Name           : constant String := To_String (Yass_Conf.Site_Name);
      Image_Description         : constant String := To_String (Yass_Conf.Description);
      Image_Language            : constant String := To_String (Yass_Conf.Language);
      Image_Author_Name         : constant String := To_String (Yass_Conf.Author_Name);
      Image_Author_Email        : constant String := To_String (Yass_Conf.Author_Email);
      Image_Base_URL            : constant String := To_String (Yass_Conf.Base_Url);
      Image_Atom_Feed_Source    : constant String :=
         To_String (Yass_Conf.Atom_Feed_Source);
      Image_Atom_Feed_Amount    : constant String := Yass_Conf.Atom_Feed_Amount'Image;
      Image_Sitemap_Enabled     : constant String :=
         To_Lower (Yass_Conf.Sitemap_Enabled'Image);
      Image_HTML_Enabled        : constant String :=
         To_Lower (Yass_Conf.HTML_Enabled'Image);
      Image_Server_Enabled      : constant String :=
         To_Lower (Yass_Conf.Server_Enabled'Image);
      Image_Server_Port         : constant String := Yass_Conf.Server_Port'Image;
      Image_Stop_Server_On_Error : constant String :=
         To_Lower (Yass_Conf.Stop_Server_On_Error'Image);
      Image_Browser_Command     : constant String :=
         To_String (Yass_Conf.Browser_Command);
      Image_Monitor_Interval    : constant String :=
         Yass_Conf.Monitor_Config_Interval'Image;
      Image_Monitor_Config_Interval : constant String :=
         Yass_Conf.Monitor_Config_Interval'Image;
      Image_Start_Tag_Separator : constant String :=
         To_String (Yass_Conf.Start_Tag_Separator);
      Image_End_Tag_Separator   : constant String :=
         To_String (Yass_Conf.End_Tag_Separator);
      Image_Markdown_Comment    : constant String :=
         To_String (Yass_Conf.Markdown_Comment);

      procedure PL (Item : String);

      procedure PL (Item : String) is
      begin
         Put_Line (File => Config_File,
                   Item => Item);
      end PL;

   begin
      Create
        (File => Config_File,
         Mode => Append_File,
         Name => Directory_Name & Dir_Separator & "site.cfg");

      PL ("# Directory in which will be placed HTML files with site layout");
      PL ("# (templates). May be absolute or relative to project directory.");
      PL ("LayoutsDirectory = _layouts");
      PL ("");

      PL ("# Directory in which will be placed generated site. May be absolute");
      PL ("# or relative to project directory.");
      PL ("OutputDirectory = _output");
      PL ("");

      PL ("# Directory in which will be placed program modules used to generate");
      PL ("# the site. May be absolute or relative to project directory.");
      PL ("ModulesDirectory = _modules");
      PL ("");

      PL ("# List of excluded files and directories from list of sources used to");
      PL ("# generating the site. All paths must be relative to the project");
      PL ("# directory. If you exclude directory, it whole content will be");
      PL ("# excluded too. Layouts, modules and output directories are excluded");
      PL ("# by default.");
      PL ("ExcludedFiles = .git,.gitignore,tags");
      PL ("");

      PL ("# The name of the site which will be created. If you have enabled");
      PL ("# creating Atom feed then it is needed. Otherwise, you can use it as a");
      PL ("# normal template tag.");
      PL ("Name = " & Image_Site_Name);
      PL ("");

      PL ("# The description of the site which will be created. Must be in one line,");
      PL ("# no new line allowed. It is used to set meta tag description (which is");
      PL ("# showed in search engines results) but only when pages don't set it.");
      PL ("# Optional setting.");
      PL ("Description = " & Image_Description);
      PL ("");

      PL ("# The ISO 639-1 language code in which the site will be created.");
      PL ("Language = " & Image_Language);
      PL ("");

      PL ("# Name of author of the site. If you have enable creating Atom feed,");
      PL ("# then it is needed. Otherwise, you can use it as a normal template tag.");
      PL ("# It is also used in setting meta tag author for all pages.");
      PL ("Author = " & Image_Author_Name);
      PL ("");

      PL ("# Email address of author of the site. If you have enable creating Atom");
      PL ("# feed, then it is needed. Otherwise, you can use it as a normal");
      PL ("# template tag.");
      PL ("AuthorEmail = " & Image_Author_Email);
      PL ("");

      PL ("# Base URL of the site. It is needed mostly for creating sitemap and");
      PL ("# Atom feed, but you can use it as a normal the site tag. If your site");
      PL ("# will be available at https://mysite.com/blog then this will be your");
      PL ("# BaseURL.");
      PL ("BaseURL = " & Image_Base_URL);
      PL ("");

      PL ("# Source which will be used for creating Atom feed of the site.");
      PL ("# Possible values are: none: don't create atom feed, tags: create");
      PL ("# Atom entries from proper tags in .md files, [filename]: the path");
      PL ("# (related to the project directory path) to markdown file which will");
      PL ("# be used as a source of atom feed (must have proper tags set inside).");
      PL ("AtomFeedSource = " & Image_Atom_Feed_Source);
      PL ("");

      PL ("# Number of entries in the Atom feed of the site. Try not set it too");
      PL ("# high, recommended values are between 10 and 50.");
      PL ("AtomFeedAmount = " & Image_Atom_Feed_Amount);
      PL ("");

      PL ("# Should the program create sitemap when creating the site. Possible");
      PL ("# values are true or false (case-insensitive).");
      PL ("SitemapEnabled = " & Image_Sitemap_Enabled);
      PL ("");

      PL ("# Should program convert HTML in markdown documents to actual HTML.");
      PL ("# Possible values are true or false (case-insensitive).");
      PL ("HTMLEnabled = " & Image_HTML_Enabled);
      PL ("");

      PL ("# Should the program start web server when monitoring for changes in");
      PL ("# site. Possible values are true or false (case-insensitive).");
      PL ("ServerEnabled = " & Image_Server_Enabled);
      PL ("");

      PL ("# Port on which web server will be listen if enabled. Possible values");
      PL ("# are from 1 to 65535. Please remember, that ports below 1025 require");
      PL ("# root privileges to work.");
      PL ("ServerPort = " & Image_Server_Port);
      PL ("");

      PL ("# Should web server and whole monitoring of the site changes stop if");
      PL ("# encounter any error during the site creation.  Possible values are");
      PL ("# true or false (case-insensitive).");
      PL ("StopServerOnError = " & Image_Stop_Server_On_Error);
      PL ("");

      PL ("# Full path to the command which will be used to start the web browser");
      PL ("# with index.html page of the site. String ""%s"" (without quotes) will");
      PL ("# be replaced by server URL. If this setting is ""none"", the web");
      PL ("# browser will be not started, same as when the web server is disabled.");
      PL ("BrowserCommand = " & Image_Browser_Command);
      PL ("");

      PL ("# How often (in seconds) the program should monitor site for changes");
      PL ("# and regenerate it if needed. Can be any positive number, but you");
      PL ("# probably don't want to set it to check every few thousands years :)");
      PL ("MonitorInterval = " & Image_Monitor_Interval);
      PL ("");

      PL ("# How often (in seconds) the program should monitor site configuration");
      PL ("# for changes and reconfigure it if needed. Can be any positive number.");
      PL ("MonitorConfigInterval = " & Image_Monitor_Config_Interval);
      PL ("");

      PL ("# String used to mark start of the templates tags, used in templates");
      PL ("# files. You may want to change it, if you want to use templates from");
      PL ("# other static site generator.");
      PL ("StartTagSeparator = " & Image_Start_Tag_Separator);
      PL ("");

      PL ("# String used to mark end of the templates tags, used in templates");
      PL ("# files. You may want to change it, if you want to use templates from");
      PL ("# other static site generator.");
      PL ("EndTagSeparator = " & Image_End_Tag_Separator);
      PL ("");

      PL ("# String used to mark comments in markdown files which will be parsed.");
      PL ("MarkdownComment = " & Image_Markdown_Comment);
      PL ("");

      PL ("# Site tags, optional. Tags can be 4 types: strings, boolean, numeric");
      PL ("# or composite.");
      PL ("# First 3 types of tags are in Name = Value scheme. For strings, it can");
      PL ("# be any alphanumeric value without new line sign. For boolean it must");
      PL ("# be ""true"" or ""false"", for numeric any number. Program will detect");
      PL ("# self which type of tag is and properly set it. It always falls back");
      PL ("# to string value.");
      PL ("# Composite tags first must be initialized with Name = [] then just");
      PL ("# add as many as you want values to it by Name = Value scheme.");
      PL ("# For more information about site.cfg file please check program");
      PL ("# documentation.");
      PL ("");

      Close (Config_File);

   end Create_Site_Config;

   ----------------------
   -- Load_Site_Config --
   ----------------------

   procedure Load_Site_Config (Directory_Name : String)
   is
      use Ada.Strings.UTF_Encoding.Strings;
      use Ada.Characters.Handling;
      use Ada.Directories;
      use Ada.Strings.Fixed;

      Config_File : File_Type;

      Raw_Data   : Unbounded_String := Null_Unbounded_String;
      Field_Name : Unbounded_String := Null_Unbounded_String;
      Value      : Unbounded_String := Null_Unbounded_String;

      Equal_Index : Natural := 0;

      procedure Normalize_Dir (Directory_Path : in out Unbounded_String);

      procedure Normalize_Dir (Directory_Path : in out Unbounded_String)
      is
      begin
         if Dir_Separator = '/'
           and then Element (Directory_Path, Index => 1) /= '/'
         then
            Directory_Path :=
              To_Unbounded_String (Directory_Name & Dir_Separator) &
              Directory_Path;
         elsif Element (Directory_Path, Index => 2) /= ':' then
            Directory_Path :=
              To_Unbounded_String (Directory_Name & Dir_Separator) &
              Directory_Path;
         end if;
      end Normalize_Dir;

   begin
      Site_Tags.Clear;
      Global_Table_Tags.Clear;

      Open (File => Config_File,
            Mode => In_File,
            Name => Directory_Name & "/site.cfg");

      Load_Configuration_Loop :
      while not End_Of_File (Config_File) loop
         Raw_Data :=
           To_Unbounded_String (Encode (Get_Line (Config_File)));

         if
           Length (Raw_Data) = 0 or else
           Element (Raw_Data, Index => 1) = '#'
         then
            goto End_Of_Loop;
         end if;

         Equal_Index := Index (Raw_Data, Pattern => "=");

         if Equal_Index = 0 then
            raise Invalid_Config_Data with To_String (Raw_Data);
         end if;

         Field_Name := Head (Raw_Data, Count => Equal_Index - 2);
         Value      := Tail (Raw_Data, Count => Length (Raw_Data) - Equal_Index - 1);

         if Field_Name = "LayoutsDirectory" then
            Yass_Conf.Layouts_Directory := Value;

         elsif Field_Name = "OutputDirectory" then
            Yass_Conf.Output_Directory := Value;

         elsif Field_Name = "ModulesDirectory" then
            Yass_Conf.Modules_Directory := Value;

         elsif Field_Name = "ExcludedFiles" then
            declare
               use GNAT.String_Split;

               Tokens : Slice_Set; --## rule line off IMPROPER_INITIALIZATION
            begin
               Create (S          => Tokens,
                       From       => To_String (Value),
                       Separators => ",");

               Add_Excluded_Files_Loop :
               for I in 1 .. Slice_Count (S => Tokens) loop
                  Yass_Conf.Excluded_Files.Append
                    (New_Item => Slice (S => Tokens, Index => I));
               end loop Add_Excluded_Files_Loop;
            end;

         elsif Field_Name = "ServerEnabled" then
            Yass_Conf.Server_Enabled :=
               To_Lower (To_String (Value)) = "true";

         elsif Field_Name = "ServerPort" then
            Yass_Conf.Server_Port :=
              Positive'Value (To_String (Value));
            if Yass_Conf.Server_Port > 65_535 then
               raise Invalid_Config_Data with To_String (Raw_Data);
            end if;

         elsif Field_Name = "StopServerOnError" then
            Yass_Conf.Stop_Server_On_Error :=
               To_Lower (Item => To_String (Value)) = "true";

         elsif Field_Name = "BrowserCommand" then
            if Index (Value, Pattern => "%s", From => 1) > 0 then
               Replace_Slice
                 (Source => Value,
                  Low    => Index (Value, Pattern => "%s", From => 1),
                  High   => Index (Value, Pattern => "%s", From => 1) + 1,
                  By     => "http://localhost:" &
                    Trim
                      (Source => Positive'Image (Yass_Conf.Server_Port),
                       Side   => Ada.Strings.Left));
            end if;
            Yass_Conf.Browser_Command := Value;

         elsif Field_Name = "MonitorInterval" then
            Yass_Conf.Monitor_Interval :=
              Duration'Value (To_String (Value));

         elsif Field_Name = "MonitorConfigInterval" then
            Yass_Conf.Monitor_Config_Interval :=
              Duration'Value (To_String (Value));

         elsif Field_Name = "BaseURL" then
            Yass_Conf.Base_Url := Value;
            Site_Tags.Include
              (Key => "BaseURL", New_Item => To_String (Value));

         elsif Field_Name = "SitemapEnabled" then
            Yass_Conf.Sitemap_Enabled :=
               To_Lower (To_String (Value)) = "true";

         elsif Field_Name = "HTMLEnabled" then
            Yass_Conf.HTML_Enabled :=
               To_Lower (To_String (Value)) = "true";

         elsif Field_Name = "AtomFeedSource" then
            if To_String (Value) in "none" | "tags" then
               Yass_Conf.Atom_Feed_Source := Value;
            else
               Yass_Conf.Atom_Feed_Source :=
                 Unbounded_Slice
                   (Source => Value, Low => 1,
                    High   => Length (Value) - 2) & "html";
            end if;

         elsif Field_Name = "AtomFeedAmount" then
            Yass_Conf.Atom_Feed_Amount :=
              Positive'Value (To_String (Value));

         elsif Field_Name = "Name" then
            Yass_Conf.Site_Name := Value;
            Site_Tags.Include (Key      => To_String (Field_Name),
                               New_Item => To_String (Value));

         elsif Field_Name = "StartTagSeparator" then
            Yass_Conf.Start_Tag_Separator := Value;

         elsif Field_Name = "EndTagSeparator" then
            Yass_Conf.End_Tag_Separator := Value;

         elsif Field_Name = "MarkdownComment" then
            Yass_Conf.Markdown_Comment := Value;

         elsif Field_Name = "Author" then
            Yass_Conf.Author_Name := Value;

         elsif Field_Name = "AuthorEmail" then
            Yass_Conf.Author_Email := Value;

         elsif Field_Name = "Language" then
            Yass_Conf.Language := Value;
            Site_Tags.Include
              (Key      => To_String (Field_Name),
               New_Item => To_String (Value));

         elsif Value = "[]" then
            Global_Table_Tags.Include (Key      => To_String (Field_Name),
                                       New_Item => +"");
            Clear (T => Global_Table_Tags (To_String (Field_Name)));

         elsif Global_Table_Tags.Contains (Key => To_String (Field_Name)) then
            Global_Table_Tags (To_String (Field_Name)) :=
              Global_Table_Tags (To_String (Field_Name)) & Value;

         else
            Site_Tags.Include
              (Key      => To_String (Field_Name),
               New_Item => To_String (Value));
         end if;

         <<End_Of_Loop>>
      end loop Load_Configuration_Loop;

      Close (Config_File);

      Normalize_Dir (Directory_Path => Yass_Conf.Layouts_Directory);
      Normalize_Dir (Directory_Path => Yass_Conf.Output_Directory);
      Normalize_Dir (Directory_Path => Yass_Conf.Modules_Directory);

      Yass_Conf.Excluded_Files.Append (".");
      Yass_Conf.Excluded_Files.Append ("..");
      Yass_Conf.Excluded_Files.Append ("site.cfg");
      Yass_Conf.Excluded_Files.Append
        (Simple_Name
          (Name => To_String (Yass_Conf.Layouts_Directory)));
      Yass_Conf.Excluded_Files.Append
        (Simple_Name
          (Name => To_String (Yass_Conf.Output_Directory)));
      Yass_Conf.Excluded_Files.Append
        (Simple_Name
          (Name => To_String (Yass_Conf.Modules_Directory)));

      Site_Directory := To_Unbounded_String (Directory_Name);

      Set_Tag_Separators
        (Start_With => To_String (Yass_Conf.Start_Tag_Separator),
         Stop_With  => To_String (Yass_Conf.End_Tag_Separator));
   exception
      when others =>
         raise Invalid_Config_Data with To_String (Raw_Data);
   end Load_Site_Config;

   --------------
   -- Ask_User --
   --------------

   function Ask_User (Default : String) return Unbounded_String;

   function Ask_User (Default : String) return Unbounded_String
   is
      Answer : Unbounded_String;
   begin
      Put ("(Default - " & Default & ")");
      Put (" > ");
      Answer := To_Unbounded_String (Get_Line);
      if Answer = "" then
         Answer := To_Unbounded_String (Default);
      end if;
      return Answer;
   end Ask_User;

   -----------------------------
   -- Interactive_Site_Config --
   -----------------------------

   procedure Interactive_Site_Config
   is
      Answer_1, Answer_2, Answer_3 : Unbounded_String;
      Answer_4, Answer_5, Answer_6 : Unbounded_String;
   begin
      Put_Line ("Now we ask you some questions about your new site.");
      Put_Line ("You can always change it later by modifying the site configuration");
      Put_Line ("file. If you just press Enter as a answer, default value will be");
      Put_Line ("used.");
      New_Line;

      Put_Line ("Please enter the name of the new site");
      New_Line;

      Yass_Conf.Site_Name := Ask_User ("New Site");
      New_Line;

      Put_Line ("Please enter the description of the new site.");
      Put_Line ("It is used to create meta tag for the website (which is showed");
      Put_Line ("in search engines results) but only if pages don't set own.");
      Put_Line ("Must be set in one line, no new line allowed.");
      New_Line;

      Yass_Conf.Description := Ask_User ("My new site");
      New_Line;

      Put_Line ("Please enter language code in which the new site will be written");
      New_Line;

      Yass_Conf.Language := Ask_User ("en");
      New_Line;

      Put_Line ("Please enter the author of the new site");
      New_Line;

      Yass_Conf.Author_Name := Ask_User ("John Doe");
      New_Line;

      Put_Line ("Please enter the contact email for the new site");
      New_Line;

      Yass_Conf.Author_Email := Ask_User ("johndoe@example.com");
      New_Line;

      Put_Line ("Please enter base URL of the new site");
      New_Line;

      Yass_Conf.Base_Url := Ask_User ("http://localhost:8888");
      New_Line;

      Put_Line ("Do you want to create Atom feed for the new site?");
      Put_Line ("If yes, you must specify source for the feed: tags - create Atom");
      Put_Line ("entries from proper tags in Markdown files, filename - the path");
      Put_Line ("(related to the project directory path) to markdown file which");
      Put_Line ("will be used as a source of atom feed (must have proper tags set");
      Put_Line ("(inside). If you press Enter, creating Atom feed will be disabled");
      New_Line;

      Yass_Conf.Atom_Feed_Source := Ask_User ("none");
      New_Line;

      if Yass_Conf.Atom_Feed_Source = "none" then
         Yass_Conf.Atom_Feed_Source := To_Unbounded_String ("25");
      else
         Put_Line ("How much maximum entries should be in the Atom feed?");
         Put_Line ("Recommended valuese are between 10 and 50.");
         New_Line;

         Yass_Conf.Atom_Feed_Amount := Integer'Value (To_String (Ask_User ("25")));
      end if;

      Put_Line ("Do you want to create sitemap file for the new site?");
      New_Line;

      Answer_1 := Ask_User ("yes");
      New_Line;

      if To_String (Answer_1) in "yes" | "y" | "" then
         Yass_Conf.Sitemap_Enabled := True;
      else
         Yass_Conf.Sitemap_Enabled := False;
      end if;

      Put_Line ("Do you want to HTML embedded in your markdown to be converted?");
      New_Line;

      Answer_2 := Ask_User ("yes");
      New_Line;

      if To_String (Answer_2) in "yes" | "y" | "" then
         Yass_Conf.HTML_Enabled := True;
      else
         Yass_Conf.HTML_Enabled := False;
      end if;

      Put_Line ("Do you want to set more technical options (like configuring");
      Put_Line ("build-in web server)?");
      New_Line;

      Answer_3 := Ask_User ("no");
      New_Line;

      if To_String (Answer_3) in "yes" | "" then

         Put_Line ("Should the program start web server when monitoring for");
         Put_Line ("changes in the site?");
         New_Line;

         Answer_4 := Ask_User ("yes");
         New_Line;

         if To_String (Answer_4) in "yes" | "y" | "" then
            Yass_Conf.Server_Enabled := True;
         else
            Yass_Conf.Server_Enabled := False;
         end if;

         Put_Line ("On which port should the web server listening?");
         Put_Line ("Possible values are from 1 to 65535. Ports below 1025 require");
         Put_Line ("root privileges.");
         New_Line;

         Yass_Conf.Server_Port := Integer'Value (To_String (Ask_User ("8888")));
         New_Line;

         Put_Line ("Should whole monitoring option stop if encounter any error");
         Put_Line ("during the site creation?");
         New_Line;

         Answer_5 := Ask_User ("yes");
         New_Line;

         if To_String (Answer_5) in "yes" | "y" | "" then
            Yass_Conf.Stop_Server_On_Error := True;
         else
            Yass_Conf.Stop_Server_On_Error := False;
         end if;

         Put_Line ("Full path to the web broser which will be started when the");
         Put_Line ("program starts in server mode.");
         New_Line;

         Yass_Conf.Browser_Command := Ask_User ("none");
         New_Line;

         Put_Line ("How often, in seconds, the program should check for changes");
         Put_Line ("in the site files?");
         New_Line;

         Yass_Conf.Monitor_Interval :=
            Duration'Value (To_String (Ask_User ("5.0")));
         New_Line;

         Put_Line ("How often, in seconds, the program should check for changes");
         Put_Line ("in the site configuration file?");
         New_Line;

         Yass_Conf.Monitor_Config_Interval :=
            Duration'Value (To_String (Ask_User ("60.0")));
         New_Line;
      end if;

      Put_Line ("Do you want to set options related to compatybility with other");
      Put_Line ("static sites generators?");
      New_Line;

      Answer_6 := Ask_User ("no");
      New_Line;

      if To_String (Answer_6) in "yes" | "y" then
         Put_Line ("What mark should be used as a start for template tag?");
         New_Line;

         Yass_Conf.Start_Tag_Separator := Ask_User ("{%");
         New_Line;

         Put_Line ("What mark should be used as an end for template tag?");
         New_Line;

         Yass_Conf.End_Tag_Separator := Ask_User ("%}");
         New_Line;

         Put_Line ("What mark should be used as a start for the comment line in");
         Put_Line ("Markdown files?");
         New_Line;

         Yass_Conf.Markdown_Comment := Ask_User ("--");
      end if;

   end Interactive_Site_Config;

end Config;
