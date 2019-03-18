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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.UTF_Encoding.Strings; use Ada.Strings.UTF_Encoding.Strings;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.String_Split; use GNAT.String_Split;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Config is

   procedure CreateConfig(DirectoryName: String) is
      ConfigFile: File_Type;
   begin
      Create
        (ConfigFile, Append_File, DirectoryName & Dir_Separator & "site.cfg");
      Put_Line
        (ConfigFile,
         "# Directory in which will be placed HTML files with site layout (templates). May be absolute or relative to project directory.");
      Put_Line(ConfigFile, "LayoutsDirectory = _layouts");
      Put_Line
        (ConfigFile,
         "# Directory in which will be placed generated site. May be absolute or relative to project directory.");
      Put_Line(ConfigFile, "OutputDirectory = _output");
      Put_Line
        (ConfigFile,
         "# Directory in which will be placed program modules used to generate the site. May be absolute or relative to project directory.");
      Put_Line(ConfigFile, "ModulesDirectory = _modules");
      Put_Line
        (ConfigFile,
         "# List of excluded files and directories from list of sources used to generating the site. All paths must be relative to the project directory. If you exclude directory, it whole content will be excluded too. Layouts, modules and output directories are excluded by default.");
      Put_Line(ConfigFile, "ExcludedFiles = .git,.gitignore,tags");
      Put_Line
        (ConfigFile,
         "# Did program should start web server when monitoring for changes in site. Possible values are true or false (case-insensitive).");
      Put_Line(ConfigFile, "ServerEnabled = true");
      Put_Line
        (ConfigFile,
         "# Port on which web server will be listen if enabled. Possible values are from 1 to 65535. Please remember, that ports below 1025 require root privileges to work.");
      Put_Line(ConfigFile, "ServerPort = 8888");
      Put_Line
        (ConfigFile,
         "# Did web server and whole monitoring of the site changes should stop if encounter any error during the site creation.  Possible values are true or false (case-insensitive).");
      Put_Line(ConfigFile, "StopServerOnError = false");
      Put_Line
        (ConfigFile,
         "# Full path to the command which will be used to start the web browser with index.html page of the site. String ""%s"" (without quotes) will be replaced by server URL. If this setting is ""none"", the web browser will be not started, same as when the web server is disabled.");
      Put_Line(ConfigFile, "BrowserCommand = none");
      Put_Line
        (ConfigFile,
         "# How often (in seconds) the program should monitor site for changes and regenerate it if needed. Can be any positive number, but you probably don't want to set it to check every few thousands years :)");
      Put_Line(ConfigFile, "MonitorInverval = 5");
      Put_Line
        (ConfigFile,
         "# How often (in seconds) the program should monitor site configuration for changes and reconfigure it if needed. Can be any positive number.");
      Put_Line(ConfigFile, "MonitorConfigInverval = 60");
      Put_Line
        (ConfigFile,
         "# Base URL of the site. It is needed mostly for creating sitemap and Atom feed, but you can use it as a normal the site tag. If your site will be available at https://mysite.com/blog then this will be your BaseURL.");
      Put_Line(ConfigFile, "BaseURL = http://localhost:8888");
      Put_Line
        (ConfigFile,
         "# Did the program should create sitemap when creating the site. Possible values are true or false (case-insensitive).");
      Put_Line(ConfigFile, "SitemapEnabled = true");
      Put_Line
        (ConfigFile,
         "# Source which will be used for creating Atom feed of the site. Possible values are: none - don't create atom feed, tags - create Atom entries from proper tags in .md files, [filename] - the path (related to the project directory path) to markdown file which will be used as a source of atom feed (must have proper tags set inside).");
      Put_Line(ConfigFile, "AtomFeedSource = none");
      Put_Line
        (ConfigFile,
         "# Number of entries in the Atom feed of the site. Try not set it too high, recommended values are between 10 and 50.");
      Put_Line(ConfigFile, "AtomFeedAmount = 25");
      Put_Line
        (ConfigFile,
         "# The name of the site which will be created. If you have enabled creating Atom feed then it is needed. Otherwise, you can use it as a normal template tag.");
      Put_Line(ConfigFile, "Name = New Site");
      Put_Line
        (ConfigFile,
         "# Name of author of the site. If you have enable creating Atom feed, then it is needed. Otherwise, you can use it as a normal template tag.");
      Put_Line(ConfigFile, "Author = John Doe");
      Put_Line
        (ConfigFile,
         "# Email address of author of the site. If you have enable creating Atom feed, then it is needed. Otherwise, you can use it as a normal template tag.");
      Put_Line(ConfigFile, "AuthorEmail = johndoe@example.com");
      Put_Line
        (ConfigFile,
         "# String used to mark start of the templates tags, used in templates files. You may want to change it, if you want to use templates from other static site generator.");
      Put_Line(ConfigFile, "StartTagSeparator = {%");
      Put_Line
        (ConfigFile,
         "# String used to mark end of the templates tags, used in templates files. You may want to change it, if you want to use templates from other static site generator.");
      Put_Line(ConfigFile, "EndTagSeparator = %}");
      Put_Line
        (ConfigFile,
         "# String used to mark comments in markdown files which will be parsed.");
      Put_Line(ConfigFile, "MarkdownComment = --");
      Put_Line
        (ConfigFile,
         "# Site tags, optional. Tags can be 4 types: strings, boolean, numeric or composite.");
      Put_Line
        (ConfigFile,
         "# First 3 types of tags are in Name = Value scheme. For strings it can be any alphanumeric value without new line sign. For boolean it must be ""true"" or ""false"", for numeric any number. Program will detect self which type of tag is and properly set it. It always fall back to string value.");
      Put_Line
        (ConfigFile,
         "# Composite tags first must be initialized with Name = [] then just add as many as you want values to it by Name = Value scheme.");
      Put_Line
        (ConfigFile,
         "# For more informations about site.cfg file please check program documentation.");
      Close(ConfigFile);
   end CreateConfig;

   procedure ParseConfig(DirectoryName: String) is
      ConfigFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex: Natural;
      Tokens: Slice_Set;
      StartTag: Unbounded_String := To_Unbounded_String("{%");
      EndTag: Unbounded_String := To_Unbounded_String("%}");
      procedure NormalizeDir(DirectoryPath: in out Unbounded_String) is
      begin
         if Dir_Separator = '/' and then Element(DirectoryPath, 1) /= '/' then
            DirectoryPath :=
              To_Unbounded_String(DirectoryName & Dir_Separator) &
              DirectoryPath;
         elsif Element(DirectoryPath, 2) /= ':' then
            DirectoryPath :=
              To_Unbounded_String(DirectoryName & Dir_Separator) &
              DirectoryPath;
         end if;
      end NormalizeDir;
   begin
      SiteTags.Clear;
      GlobalTableTags.Clear;
      Open(ConfigFile, In_File, DirectoryName & "/site.cfg");
      while not End_Of_File(ConfigFile) loop
         RawData := To_Unbounded_String(Encode(Get_Line(ConfigFile)));
         if Length(RawData) = 0 or else Element(RawData, 1) = '#' then
            goto End_Of_Loop;
         end if;
         EqualIndex := Index(RawData, "=");
         if EqualIndex = 0 then
            raise InvalidConfigData with To_String(RawData);
         end if;
         FieldName := Head(RawData, EqualIndex - 2);
         Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
         if FieldName = To_Unbounded_String("LayoutsDirectory") then
            YassConfig.LayoutsDirectory := Value;
         elsif FieldName = To_Unbounded_String("OutputDirectory") then
            YassConfig.OutputDirectory := Value;
         elsif FieldName = To_Unbounded_String("ModulesDirectory") then
            YassConfig.ModulesDirectory := Value;
         elsif FieldName = To_Unbounded_String("ExcludedFiles") then
            Create(Tokens, To_String(Value), ",");
            for I in 1 .. Slice_Count(Tokens) loop
               YassConfig.ExcludedFiles.Append(Slice(Tokens, I));
            end loop;
         elsif FieldName = To_Unbounded_String("ServerEnabled") then
            if To_Lower(To_String(Value)) = "true" then
               YassConfig.ServerEnabled := True;
            else
               YassConfig.ServerEnabled := False;
            end if;
         elsif FieldName = To_Unbounded_String("ServerPort") then
            YassConfig.ServerPort := Positive'Value(To_String(Value));
            if YassConfig.ServerPort > 65535 then
               raise InvalidConfigData with To_String(RawData);
            end if;
         elsif FieldName = To_Unbounded_String("StopServerOnError") then
            if To_Lower(To_String(Value)) = "true" then
               YassConfig.StopServerOnError := True;
            else
               YassConfig.StopServerOnError := False;
            end if;
         elsif FieldName = To_Unbounded_String("BrowserCommand") then
            if Index(Value, "%s", 1) > 0 then
               Replace_Slice
                 (Value, Index(Value, "%s", 1), Index(Value, "%s", 1) + 1,
                  "http://localhost:" &
                  Trim
                    (Positive'Image(YassConfig.ServerPort), Ada.Strings.Both));
            end if;
            YassConfig.BrowserCommand := Value;
         elsif FieldName = To_Unbounded_String("MonitorInterval") then
            YassConfig.MonitorInterval := Duration'Value(To_String(Value));
         elsif FieldName = To_Unbounded_String("MonitorConfigInterval") then
            YassConfig.MonitorConfigInterval :=
              Duration'Value(To_String(Value));
         elsif FieldName = To_Unbounded_String("BaseURL") then
            YassConfig.BaseURL := Value;
            Tags_Container.Include(SiteTags, "BaseURL", To_String(Value));
         elsif FieldName = To_Unbounded_String("SitemapEnabled") then
            if To_Lower(To_String(Value)) = "true" then
               YassConfig.SitemapEnabled := True;
            else
               YassConfig.SitemapEnabled := False;
            end if;
         elsif FieldName = To_Unbounded_String("AtomFeedSource") then
            if Value = To_Unbounded_String("none") or
              Value = To_Unbounded_String("tags") then
               YassConfig.AtomFeedSource := Value;
            else
               YassConfig.AtomFeedSource :=
                 Unbounded_Slice(Value, 1, Length(Value) - 2) &
                 To_Unbounded_String("html");
            end if;
         elsif FieldName = To_Unbounded_String("Name") then
            YassConfig.SiteName := Value;
            Tags_Container.Include
              (SiteTags, To_String(FieldName), To_String(Value));
         elsif FieldName = To_Unbounded_String("StartTagSeparator") then
            StartTag := Value;
         elsif FieldName = To_Unbounded_String("EndTagSeparator") then
            EndTag := Value;
         elsif FieldName = To_Unbounded_String("MarkdownComment") then
            YassConfig.MarkdownComment := Value;
         elsif FieldName = To_Unbounded_String("Author") then
            YassConfig.AuthorName := Value;
         elsif FieldName = To_Unbounded_String("AuthorEmail") then
            YassConfig.AuthorEmail := Value;
         elsif Value = To_Unbounded_String("[]") then
            TableTags_Container.Include
              (GlobalTableTags, To_String(FieldName), +"");
            Clear(GlobalTableTags(To_String(FieldName)));
         elsif TableTags_Container.Contains
             (GlobalTableTags, To_String(FieldName)) then
            GlobalTableTags(To_String(FieldName)) :=
              GlobalTableTags(To_String(FieldName)) & Value;
         else
            Tags_Container.Include
              (SiteTags, To_String(FieldName), To_String(Value));
         end if;
         <<End_Of_Loop>>
      end loop;
      Close(ConfigFile);
      NormalizeDir(YassConfig.LayoutsDirectory);
      NormalizeDir(YassConfig.OutputDirectory);
      NormalizeDir(YassConfig.ModulesDirectory);
      YassConfig.ExcludedFiles.Append(".");
      YassConfig.ExcludedFiles.Append("..");
      YassConfig.ExcludedFiles.Append("site.cfg");
      YassConfig.ExcludedFiles.Append
        (Simple_Name(To_String(YassConfig.LayoutsDirectory)));
      YassConfig.ExcludedFiles.Append
        (Simple_Name(To_String(YassConfig.OutputDirectory)));
      YassConfig.ExcludedFiles.Append
        (Simple_Name(To_String(YassConfig.ModulesDirectory)));
      SiteDirectory := To_Unbounded_String(DirectoryName);
      Set_Tag_Separators(To_String(StartTag), To_String(EndTag));
   exception
      when others =>
         raise InvalidConfigData with To_String(RawData);
   end ParseConfig;

end Config;
