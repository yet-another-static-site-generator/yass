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

with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;
with AWS.Net;
with AWS.Server;
with AtomFeed;
with Config; use Config;
with Layouts; use Layouts;
with Messages; use Messages;
with Modules;
with Pages; use Pages;
with Sitemaps;
with Server; use Server;

procedure Yass is
   Version: constant String := "3.0";
   Work_Directory: Unbounded_String := Null_Unbounded_String;

   -- ****if* YASS/Build_Site
   -- FUNCTION
   -- Build the site from directory
   -- PARAMETERS
   -- Directory_Name - full path to the site directory
   -- RESULT
   -- Returns True if the site was build, otherwise False.
   -- SOURCE
   function Build_Site(Directory_Name: String) return Boolean is
      -- ****
      use AtomFeed;
      use Modules;
      use Sitemaps;

      Page_Tags: Tags_Container.Map := Tags_Container.Empty_Map;
      Page_Table_Tags: TableTags_Container.Map :=
        TableTags_Container.Empty_Map;
      -- Build the site from directory with full path Name
      procedure Build(Name: String) is
         -- Process file with full path Item: create html pages from markdown files or copy any other file.
         procedure Process_Files(Item: Directory_Entry_Type) is
         begin
            if Yass_Config.Excluded_Files.Find_Index
                (Item => Simple_Name(Directory_Entry => Item)) /=
              Excluded_Container.No_Index or
              not Ada.Directories.Exists
                (Name => Full_Name(Directory_Entry => Item)) then
               return;
            end if;
            Set
              (Name => "YASSFILE",
               Value => Full_Name(Directory_Entry => Item));
            if Extension(Name => Simple_Name(Directory_Entry => Item)) =
              "md" then
               CreatePage
                 (FileName => Full_Name(Directory_Entry => Item),
                  Directory => Name);
            else
               CopyFile
                 (FileName => Full_Name(Directory_Entry => Item),
                  Directory => Name);
            end if;
         end Process_Files;
         -- Go recursive with directory with full path Item.
         procedure Process_Directories(Item: Directory_Entry_Type) is
         begin
            if Yass_Config.Excluded_Files.Find_Index
                (Item => Simple_Name(Directory_Entry => Item)) =
              Excluded_Container.No_Index and
              Ada.Directories.Exists
                (Name => Full_Name(Directory_Entry => Item)) then
               Build(Name => Full_Name(Directory_Entry => Item));
            end if;
         exception
            when Ada.Directories.Name_Error =>
               null;
         end Process_Directories;
      begin
         Search
           (Directory => Name, Pattern => "",
            Filter => (Directory => False, others => True),
            Process => Process_Files'Access);
         Search
           (Directory => Name, Pattern => "",
            Filter => (Directory => True, others => False),
            Process => Process_Directories'Access);
      end Build;
   begin
      -- Load the program modules with 'start' hook
      LoadModules
        (State => "start", PageTags => Page_Tags,
         PageTableTags => Page_Table_Tags);
      -- Load data from exisiting sitemap or create new set of data or nothing if sitemap generation is disabled
      StartSitemap;
      -- Load data from existing atom feed or create new set of data or nothing if atom feed generation is disabled
      Start_Atom_Feed;
      -- Build the site
      Build(Name => Directory_Name);
      -- Save atom feed to file or nothing if atom feed generation is disabled
      Save_Atom_Feed;
      -- Save sitemap to file or nothing if sitemap generation is disabled
      SaveSitemap;
      -- Load the program modules with 'end' hook
      LoadModules
        (State => "end", PageTags => Page_Tags,
         PageTableTags => Page_Table_Tags);
      return True;
   exception
      when GenerateSiteException =>
         return False;
   end Build_Site;

   -- ****if* YASS/Valid_Arguments
   -- FUNCTION
   -- Validate arguments which user was entered when started the program and
   -- set Work_Directory for the program.
   -- PARAMETERS
   -- Message - part of message to show when user does not entered the site
   --           project directory
   -- Exist   - did selected directory should be test did it exist or not
   -- RESULT
   -- Returns True if entered arguments are valid, otherwise False.
   -- SOURCE
   function Valid_Arguments(Message: String; Exist: Boolean) return Boolean is
   -- ****
   begin
      -- User does not entered name of the site project directory
      if Argument_Count < 2 then
         ShowMessage(Text => "Please specify directory name " & Message);
         return False;
      end if;
      -- Assign Work_Directory
      if Index
          (Source => Argument(Number => 2),
           Pattern => Containing_Directory(Name => Current_Directory)) =
        1 then
         Work_Directory :=
           To_Unbounded_String(Source => Argument(Number => 2));
      else
         Work_Directory :=
           To_Unbounded_String
             (Source =>
                Current_Directory & Dir_Separator & Argument(Number => 2));
      end if;
      -- Check if selected directory exist, if not, return False
      if Ada.Directories.Exists(Name => To_String(Source => Work_Directory)) =
        Exist then
         if Exist then
            ShowMessage
              (Text =>
                 "Directory with that name exists, please specify another.");
         else
            ShowMessage
              (Text =>
                 "Directory with that name not exists, please specify existing site directory.");
         end if;
         return False;
      end if;
      -- Check if selected directory is valid the program site project directory. Return False if not.
      if not Exist and
        not Ada.Directories.Exists
          (Name =>
             To_String(Source => Work_Directory) & Dir_Separator &
             "site.cfg") then
         ShowMessage
           (Text =>
              "Selected directory don't have file ""site.cfg"". Please specify proper directory.");
         return False;
      end if;
      return True;
   end Valid_Arguments;

   -- ****if* YASS/Show_Help
   -- FUNCTION
   -- Show the program help - list of available commands
   -- SOURCE
   procedure Show_Help is
   -- ****
   begin
      Put_Line(Item => "Possible actions:");
      Put_Line(Item => "help - show this screen and exit");
      Put_Line(Item => "version - show the program version and exit");
      Put_Line(Item => "license - show short info about the program license");
      Put_Line(Item => "readme - show content of README file");
      Put_Line
        (Item => "createnow [name] - create new site in ""name"" directory");
      Put_Line
        (Item =>
           "create [name] - interactively create new site in ""name"" directory");
      Put_Line(Item => "build [name] - build site in ""name"" directory");
      Put_Line
        (Item =>
           "server [name] - start simple HTTP server in ""name"" directory and auto rebuild site if needed.");
      Put_Line
        (Item =>
           "createfile [name] - create new empty markdown file with ""name""");
   end Show_Help;

   procedure Create is
   begin
      if not Valid_Arguments
          (Message => "where new page will be created.", Exist => True) then
         return;
      end if;
      Create_Directories_Block :
      declare
         Paths: constant array(1 .. 6) of Unbounded_String :=
           (1 => To_Unbounded_String(Source => "_layouts"),
            2 => To_Unbounded_String(Source => "_output"),
            3 =>
              To_Unbounded_String
                (Source => "_modules" & Dir_Separator & "start"),
            4 =>
              To_Unbounded_String
                (Source => "_modules" & Dir_Separator & "pre"),
            5 =>
              To_Unbounded_String
                (Source => "_modules" & Dir_Separator & "post"),
            6 =>
              To_Unbounded_String
                (Source => "_modules" & Dir_Separator & "end"));
      begin
         Create_Directories_Loop :
         for Directory of Paths loop
            Create_Path
              (New_Directory =>
                 To_String(Source => Work_Directory) & Dir_Separator &
                 To_String(Source => Directory));
         end loop Create_Directories_Loop;
      end Create_Directories_Block;
      if Argument(Number => 1) = "create" then
         CreateInteractiveConfig
           (DirectoryName => To_String(Source => Work_Directory));
      else
         CreateConfig(DirectoryName => To_String(Source => Work_Directory));
      end if;
      CreateLayout(DirectoryName => To_String(Source => Work_Directory));
      CreateDirectoryLayout
        (DirectoryName => To_String(Source => Work_Directory));
      CreateEmptyFile(FileName => To_String(Source => Work_Directory));
      ShowMessage
        (Text =>
           "New page in directory """ & Argument(Number => 2) &
           """ was created. Edit """ & Argument(Number => 2) & Dir_Separator &
           "site.cfg"" file to set data for your new site.",
         MType => Messages.Success);
   end Create;

begin
   if Ada.Environment_Variables.Exists(Name => "YASSDIR") then
      Set_Directory(Directory => Value(Name => "YASSDIR"));
   end if;
   -- No arguments or help: show available commands
   if Argument_Count < 1 or else Argument(Number => 1) = "help" then
      Show_Help;
      -- Show version information
   elsif Argument(Number => 1) = "version" then
      Put_Line(Item => "Version: " & Version);
      Put_Line(Item => "Released: not yet");
      -- Show license information
   elsif Argument(Number => 1) = "license" then
      Put_Line(Item => "Copyright (C) 2019-2021 Bartek thindil Jasicki");
      New_Line;
      Put_Line
        (Item =>
           "This program is free software: you can redistribute it and/or modify");
      Put_Line
        (Item =>
           "it under the terms of the GNU General Public License as published by");
      Put_Line
        (Item =>
           "the Free Software Foundation, either version 3 of the License, or");
      Put_Line(Item => "(at your option) any later version.");
      New_Line;
      Put_Line
        (Item =>
           "This program is distributed in the hope that it will be useful,");
      Put_Line
        (Item =>
           "but WITHOUT ANY WARRANTY; without even the implied warranty of");
      Put_Line
        (Item =>
           "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the");
      Put_Line(Item => "GNU General Public License for more details.");
      New_Line;
      Put_Line
        (Item =>
           "You should have received a copy of the GNU General Public License");
      Put_Line
        (Item =>
           "along with this program.  If not, see <https://www.gnu.org/licenses/>.");
      -- Show README.md file
   elsif Argument(Number => 1) = "readme" then
      Show_Readme_Block :
      declare
         Readme_Name: constant String :=
           (if Ada.Environment_Variables.Exists(Name => "APPDIR") then
              Value(Name => "APPDIR") & "/usr/share/doc/yass/README.md"
            else Containing_Directory(Name => Command_Name) & Dir_Separator &
              "README.md");
         Readme_File: File_Type;
      begin
         if not Ada.Directories.Exists(Name => Readme_Name) then
            ShowMessage(Text => "Can't find file " & Readme_Name);
            return;
         end if;
         Open(File => Readme_File, Mode => In_File, Name => Readme_Name);
         Show_Readme_Loop :
         while not End_Of_File(File => Readme_File) loop
            Put_Line(Item => Get_Line(File => Readme_File));
         end loop Show_Readme_Loop;
         Close(File => Readme_File);
      end Show_Readme_Block;
      -- Create new, selected site project directory
   elsif Argument(Number => 1) in "createnow" | "create" then
      Create;
   elsif Argument(Number => 1) = "build" then
      if not Valid_Arguments
          (Message => "from where page will be created.", Exist => False) then
         return;
      end if;
      ParseConfig(DirectoryName => To_String(Source => Work_Directory));
      if Build_Site(Directory_Name => To_String(Source => Work_Directory)) then
         ShowMessage(Text => "Site was build.", MType => Messages.Success);
      else
         ShowMessage(Text => "Site building has been interrupted.");
      end if;
      -- Start server to monitor changes in selected site project
   elsif Argument(Number => 1) = "server" then
      if not Valid_Arguments
          (Message => "from where site will be served.", Exist => False) then
         return;
      end if;
      ParseConfig(DirectoryName => To_String(Source => Work_Directory));
      if not Ada.Directories.Exists
          (Name => To_String(Source => Yass_Config.Output_Directory)) then
         Create_Path
           (New_Directory =>
              To_String(Source => Yass_Config.Output_Directory));
      end if;
      Set_Directory
        (Directory => To_String(Source => Yass_Config.Output_Directory));
      if Yass_Config.Server_Enabled then
         if not Ada.Directories.Exists
             (Name =>
                To_String(Source => Yass_Config.Layouts_Directory) &
                Dir_Separator & "directory.html") then
            CreateDirectoryLayout(DirectoryName => "");
         end if;
         StartServer;
         if Yass_Config.Browser_Command /=
           To_Unbounded_String(Source => "none") then
            Start_Web_Browser_Block :
            declare
               Args: constant Argument_List_Access :=
                 Argument_String_To_List
                   (Arg_String =>
                      To_String(Source => Yass_Config.Browser_Command));
            begin
               if not Ada.Directories.Exists(Name => Args(Args'First).all)
                 or else
                   Non_Blocking_Spawn
                     (Program_Name => Args(Args'First).all,
                      Args => Args(Args'First + 1 .. Args'Last)) =
                   Invalid_Pid then
                  Put_Line
                    (Item =>
                       "Can't start web browser. Please check your site configuration did it have proper value for ""BrowserCommand"" setting.");
                  ShutdownServer;
                  return;
               end if;
            end Start_Web_Browser_Block;
         end if;
      else
         Put_Line
           (Item => "Started monitoring site changes. Press ""Q"" for quit.");
      end if;
      MonitorSite.Start;
      MonitorConfig.Start;
      AWS.Server.Wait(Mode => AWS.Server.Q_Key_Pressed);
      if Yass_Config.Server_Enabled then
         ShutdownServer;
      else
         Put(Item => "Stopping monitoring site changes...");
      end if;
      abort MonitorSite;
      abort MonitorConfig;
      ShowMessage(Text => "done.", MType => Messages.Success);
      -- Create new empty markdown file with selected name
   elsif Argument(Number => 1) = "createfile" then
      if Argument_Count < 2 then
         ShowMessage(Text => "Please specify name of file to create.");
         return;
      end if;
      if Index
          (Source => Argument(Number => 2),
           Pattern => Containing_Directory(Name => Current_Directory)) =
        1 then
         Work_Directory :=
           To_Unbounded_String(Source => Argument(Number => 2));
      else
         Work_Directory :=
           To_Unbounded_String
             (Source =>
                Current_Directory & Dir_Separator & Argument(Number => 2));
      end if;
      if Extension(Name => To_String(Source => Work_Directory)) /= "md" then
         Work_Directory :=
           Work_Directory & To_Unbounded_String(Source => ".md");
      end if;
      if Ada.Directories.Exists
          (Name => To_String(Source => Work_Directory)) then
         Put_Line
           (Item =>
              "Can't create file """ & To_String(Source => Work_Directory) &
              """. File with that name exists.");
         return;
      end if;
      Create_Path
        (New_Directory =>
           Containing_Directory(Name => To_String(Source => Work_Directory)));
      CreateEmptyFile(FileName => To_String(Source => Work_Directory));
      ShowMessage
        (Text =>
           "Empty file """ & To_String(Source => Work_Directory) &
           """ was created.",
         MType => Messages.Success);
      -- Unknown command entered
   else
      ShowMessage(Text => "Unknown command '" & Argument(Number => 1) & "'");
      Show_Help;
   end if;
exception
   when An_Exception : InvalidConfigData =>
      ShowMessage
        (Text =>
           "Invalid data in site config file ""site.cfg"". Invalid line:""" &
           Exception_Message(X => An_Exception) & """");
   when AWS.Net.Socket_Error =>
      ShowMessage
        (Text =>
           "Can't start program in server mode. Probably another program is using this same port, or you have still connected old instance of the program in your browser. Please close whole browser and try run the program again. If problem will persist, try to change port for the server in the site configuration.");
   when An_Exception : others =>
      Save_Exception_Info_Block :
      declare
         use Ada.Calendar;
         use GNAT.Traceback.Symbolic;

         Error_File: File_Type;
      begin
         if Ada.Directories.Exists(Name => "error.log") then
            Open(File => Error_File, Mode => Append_File, Name => "error.log");
         else
            Create
              (File => Error_File, Mode => Append_File, Name => "error.log");
         end if;
         Put_Line
           (File => Error_File,
            Item => Ada.Calendar.Formatting.Image(Date => Clock));
         Put_Line(File => Error_File, Item => Version);
         Put_Line
           (File => Error_File,
            Item => "Exception: " & Exception_Name(X => An_Exception));
         Put_Line
           (File => Error_File,
            Item => "Message: " & Exception_Message(X => An_Exception));
         Put_Line
           (File => Error_File,
            Item => "-------------------------------------------------");
         if Directory_Separator = '/' then
            Put_Line
              (File => Error_File,
               Item => Symbolic_Traceback(E => An_Exception));
         else
            Put_Line
              (File => Error_File,
               Item => Exception_Information(X => An_Exception));
         end if;
         Put_Line
           (File => Error_File,
            Item => "-------------------------------------------------");
         Close(File => Error_File);
         Put_Line
           (Item =>
              "Oops, something bad happen and program crashed. Please, remember what you done before crash and report this problem at https://www.laeran.pl/repositories/yass and attach (if possible) file 'error.log' (should be in this same directory).");
      end Save_Exception_Info_Block;
end Yass;
