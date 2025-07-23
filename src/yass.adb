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

with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;

with AWS.Net;
with AWS.Server;

with Resources;

with AtomFeed;
with Config;
with Layouts;
with Messages;
with Modules;
with Monitors;
with Pages;
with Sitemaps;
with Server;

procedure Yass
is
   use Ada.Strings.Unbounded;

   Version  : constant String := "3.2.0-dev";  --  Keep in sync with alire.toml
   Released : constant String := "2024-08-23";

   Dir_Separator : Character renames GNAT.Directory_Operations.Dir_Separator;

   --## rule off GLOBAL_REFERENCES
   Work_Directory : Unbounded_String := Null_Unbounded_String;
   --## rule on GLOBAL_REFERENCES

   -- ****if* YASS/YASS.Build_Site
   -- FUNCTION
   -- Build the site from directory
   -- PARAMETERS
   -- Directory_Name - full path to the site directory
   -- RESULT
   -- Returns True if the site was build, otherwise False.
   -- SOURCE
   function Build_Site (Directory_Name : String) return Boolean
   with
      Pre => Directory_Name'Length > 0;
   -- ****

   ----------------
   -- Build_Site --
   ----------------

   function Build_Site (Directory_Name : String) return Boolean
   is
      use Ada.Directories;

      use AtomFeed;
      use Config;
      use Modules;
      use Sitemaps;

      Page_Tags       : Tags_Container.Map := Tags_Container.Empty_Map;
      Page_Table_Tags : TableTags_Container.Map :=
        TableTags_Container.Empty_Map;

      procedure Build (Name : String)
      with
         Pre => Name'Length > 0;
      --  Build the site from directory with full path Name

      procedure Build (Name : String)
      is
         procedure Process_Files (Item : Directory_Entry_Type);
         --  Process file with full path Item: create html pages from markdown
         --  files or copy any other file.

         procedure Process_Directories (Item : Directory_Entry_Type);
         --  Go recursive with directory with full path Item.

         -------------------
         -- Process_Files --
         -------------------

         procedure Process_Files (Item : Directory_Entry_Type) is
         begin
            if
              Yass_Conf.Excluded_Files.Find_Index
                (Item => Simple_Name (Directory_Entry => Item)) /=
              Excluded_Container.No_Index or
              not Ada.Directories.Exists
                (Name => Full_Name (Directory_Entry => Item))
            then
               return;
            end if;

            Ada.Environment_Variables.Set
              (Name  => "YASSFILE",
               Value => Full_Name (Directory_Entry => Item));

            if Extension (Name => Simple_Name (Directory_Entry => Item)) = "md"
            then
               Pages.Create_Page
                 (File_Name => Full_Name (Directory_Entry => Item),
                  Directory => Name);
            else
               Pages.Copy_File
                 (File_Name => Full_Name (Directory_Entry => Item),
                  Directory => Name);
            end if;

         end Process_Files;

         -------------------------
         -- Process_Directories --
         -------------------------

         procedure Process_Directories (Item : Directory_Entry_Type) is
         begin
            if
              Yass_Conf.Excluded_Files.Find_Index
                (Item => Simple_Name (Directory_Entry => Item)) =
              Excluded_Container.No_Index and
              Ada.Directories.Exists
                (Name => Full_Name (Directory_Entry => Item))
            then
               Build (Name => Full_Name (Directory_Entry => Item));
            end if;

         exception
            when Ada.Directories.Name_Error =>
               null;
         end Process_Directories;

      begin
         Search
           (Directory => Name,
            Pattern   => "",
            Filter    => (Directory => False, others => True),
            Process   => Process_Files'Access);
         Search
           (Directory => Name,
            Pattern   => "",
            Filter    => (Directory => True, others => False),
            Process   => Process_Directories'Access);
      end Build;
   begin
      --  Load the program modules with 'start' hook
      Load_Modules (State           => "start",
                    Page_Tags       => Page_Tags,
                    Page_Table_Tags => Page_Table_Tags);

      --  Load data from exisiting sitemap or create new set of data or nothing
      --  if sitemap generation is disabled
      Start_Sitemap;

      --  Load data from existing atom feed or create new set of data or nothing
      --  if atom feed generation is disabled
      Start_Atom_Feed;

      --  Build the site
      Build (Name => Directory_Name);

      --  Save atom feed to file or nothing if atom feed generation is disabled
      Save_Atom_Feed;

      --  Save sitemap to file or nothing if sitemap generation is disabled
      Save_Sitemap;

      --  Load the program modules with 'end' hook
      Load_Modules (State           => "end",
                    Page_Tags       => Page_Tags,
                    Page_Table_Tags => Page_Table_Tags);
      return True;

   exception
      when Pages.Generate_Site_Exception =>
         return False;
   end Build_Site;

   -- ****if* YASS/YASS.Valid_Arguments
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
   function Valid_Arguments (Message : String;
                             Exist   : Boolean) return Boolean
   with
      Pre => Message'Length > 0;
   -- ****

   ---------------------
   -- Valid_Arguments --
   ---------------------

   function Valid_Arguments (Message : String;
                             Exist   : Boolean) return Boolean
   is
      use Ada.Command_Line;
      use Ada.Directories;
      use Messages;
   begin
      -- User does not entered name of the site project directory
      if Argument_Count < 2 then
         Show_Message (Text => "Please specify directory name " & Message);
         return False;
      end if;

      --  Assign Work_Directory
      declare
         Path : String renames Argument (Number => 2);
      begin
         Work_Directory := To_Unbounded_String (Full_Name (Path));
      end;

      --   Check if selected directory exist, if not, return False
      if Ada.Directories.Exists (Name => To_String (Work_Directory)) = Exist then
         if Exist then
            Show_Message
              (Text =>
                 "Directory with that name exists, please specify another.");
         else
            Show_Message
              (Text =>
                 "Directory with that name not exists, please specify existing "
                 & "site directory.");
         end if;
         return False;
      end if;

      --  Check if selected directory is valid the program site project directory.
      --  Return False if not.
      if not Exist and not Ada.Directories.Exists
          (Name => To_String (Work_Directory) & Dir_Separator & "site.cfg")
      then
         Show_Message
           (Text =>
              "Selected directory don't have file ""site.cfg"". Please specify "
              & "proper directory.");
         return False;
      end if;
      return True;
   end Valid_Arguments;

   -- ****if* YASS/YASS.Show_Help
   -- FUNCTION
   -- Show the program help - list of available commands
   -- SOURCE
   procedure Show_Help;
   -- ****

   ---------------
   -- Show_Help --
   ---------------

   procedure Show_Help is
      use Ada.Text_IO;
   begin
      Put_Line (Item => "Possible actions:");
      Put_Line (Item => "help - show this screen and exit");
      Put_Line (Item => "version - show the program version and exit");
      Put_Line (Item => "license - show short info about the program license");
      Put_Line (Item => "readme - show content of README file");
      Put_Line
        (Item => "createnow [name] - create new site in ""name"" directory");
      Put_Line
        (Item =>
           "create [name] - interactively create new site in ""name"" directory");
      Put_Line (Item => "build [name] - build site in ""name"" directory");
      Put_Line
        (Item =>
           "server [name] - start simple HTTP server in ""name"" directory and "
           & "auto rebuild site if needed.");
      Put_Line
        (Item =>
           "createfile [name] - create new empty markdown file with ""name""");
   end Show_Help;

   ------------
   -- Create --
   ------------

   procedure Create;

   procedure Create
   is
      use Ada.Command_Line;
      use Ada.Directories;
      use Config;
   begin
      if not Valid_Arguments
          (Message => "where new page will be created.", Exist => True)
      then
         return;
      end if;

      declare
         Path  : String renames To_String (Work_Directory);
         Paths : constant array (1 .. 6) of Unbounded_String :=
           (1 => To_Unbounded_String ("_layouts"),
            2 => To_Unbounded_String ("_output"),
            3 => To_Unbounded_String ("_modules" & Dir_Separator & "start"),
            4 => To_Unbounded_String ("_modules" & Dir_Separator & "pre"),
            5 => To_Unbounded_String ("_modules" & Dir_Separator & "post"),
            6 => To_Unbounded_String ("_modules" & Dir_Separator & "end"));
      begin
         Create_Directories_Loop :
         for Directory of Paths loop
            Create_Path (Path & Dir_Separator & To_String (Directory));
         end loop Create_Directories_Loop;

         if Argument (Number => 1) = "create" then
            Interactive_Site_Config;
         end if;
         Create_Site_Config (Directory_Name => To_String (Work_Directory));

         Layouts.Create_Layout           (Directory_Name => Path);
         Layouts.Create_Directory_Layout (Directory_Name => Path);
         Pages.Create_Empty_File         (File_Name      => Path);

         Messages.Show_Message
           ("New page in directory """ & Path &
            """ was created. Edit """ & Path & Dir_Separator &
            "site.cfg"" file to set data for your new site.",
            Message_Type => Messages.SUCCESS);
      end;
   end Create;

   use Ada.Command_Line;
   use Ada.Directories;
   use Ada.Text_IO;
   use Config;
begin
   if Ada.Environment_Variables.Exists (Name => "YASSDIR") then
      Set_Directory
         (Directory => Ada.Environment_Variables.Value (Name => "YASSDIR"));
   end if;

   --  No arguments or help: show available commands
   if Argument_Count < 1 or else Argument (Number => 1) in "help" | "--help" then
      Show_Help;

   --  Show version information
   elsif Argument (Number => 1) in "version" | "--version" then
      Put_Line ("Version: " & Version);
      Put_Line ("Released: " & Released);

   --  Show license information
   elsif Argument (Number => 1) = "license" then
      Put_Line (Item => "Copyright (C) 2022-2024 A.J. Ianozi");
      Put_Line (Item => "Copyright (C) 2019-2021 Bartek thindil Jasicki");
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
      Put_Line (Item => "(at your option) any later version.");
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
      Put_Line (Item => "GNU General Public License for more details.");
      New_Line;
      Put_Line
        (Item =>
           "You should have received a copy of the GNU General Public License");
      Put_Line
        (Item =>
           "along with this program.  If not, see <https://www.gnu.org/licenses/>.");

   --  Show README.md file
   elsif Argument (Number => 1) = "readme" then

      Show_Readme_Block :
      declare
         package Yass_Resources
           is new Resources (Crate_Name => "yass");

         Readme_Name : constant String :=
           Yass_Resources.Resource_Path & Dir_Separator & "README.md";

         Readme_File : File_Type;
      begin
         if not Ada.Directories.Exists (Name => Readme_Name) then
            Messages.Show_Message (Text => "Can't find file " & Readme_Name);
            return;
         end if;

         Open (File => Readme_File,
               Mode => In_File,
               Name => Readme_Name);

         Show_Readme_Loop :
         while not End_Of_File (Readme_File) loop
            Put_Line (Item => Get_Line (Readme_File));
         end loop Show_Readme_Loop;

         Close (Readme_File);

      end Show_Readme_Block;

   --  Create new, selected site project directory
   elsif Argument (Number => 1) in "createnow" | "create" then
      Create;

   elsif Argument (Number => 1) = "build" then
      if not Valid_Arguments
          (Message => "from where page will be created.", Exist => False)
      then
         return;
      end if;

      Load_Site_Config (Directory_Name => To_String (Work_Directory));

      if Build_Site (Directory_Name => To_String (Work_Directory)) then
         Messages.Show_Message (Text         => "Site was build.",
                                Message_Type => Messages.SUCCESS);
      else
         Messages.Show_Message (Text => "Site building has been interrupted.");
      end if;

   --  Start server to monitor changes in selected site project
   elsif Argument (Number => 1) = "server" then
      if not Valid_Arguments
          (Message => "from where site will be served.", Exist => False)
      then
         return;
      end if;

      Load_Site_Config (Directory_Name => To_String (Work_Directory));

      if not Ada.Directories.Exists
          (Name => To_String (Yass_Conf.Output_Directory))
      then
         Create_Path
           (New_Directory => To_String (Yass_Conf.Output_Directory));
      end if;

      Set_Directory (Directory => To_String (Yass_Conf.Output_Directory));

      if Yass_Conf.Server_Enabled then
         if not Ada.Directories.Exists
             (Name =>
                To_String (Yass_Conf.Layouts_Directory) &
                Dir_Separator & "directory.html")
         then
            Layouts.Create_Directory_Layout (Directory_Name => "");
         end if;

         Server.Start_Server;

         if Yass_Conf.Browser_Command /= To_Unbounded_String ("none") then

            Start_Web_Browser_Block :
            declare
               use GNAT.OS_Lib;

               Args : Argument_List_Access :=
                 Argument_String_To_List
                   (Arg_String => To_String (Yass_Conf.Browser_Command));
            begin
               if not Ada.Directories.Exists (Name => Args (Args'First).all)
                 or else
                   Non_Blocking_Spawn
                     (Program_Name => Args (Args'First).all,
                      Args         => Args (Args'First + 1 .. Args'Last)) =
                   Invalid_Pid
               then
                  Put_Line
                    (Item =>
                       "Can't start web browser. Please check your site "
                       & "configuration did it have proper value for "
                       & """BrowserCommand"" setting.");

                  Server.Shutdown_Server;
                  return;

               end if;

               Free (Args);
            end Start_Web_Browser_Block;

         end if;

      else
         Put_Line
           (Item => "Started monitoring site changes. Press ""Q"" for quit.");
      end if;

      Monitors.Monitor_Site.Start;
      Monitors.Monitor_Config.Start;

      AWS.Server.Wait (Mode => AWS.Server.Q_Key_Pressed);
      if Yass_Conf.Server_Enabled then
         Server.Shutdown_Server;
      else
         Put (Item => "Stopping monitoring site changes...");
      end if;

      Monitors.Monitor_Site.Stop;
      Monitors.Monitor_Config.Stop;

      Messages.Show_Message (Text         => "done.",
                             Message_Type => Messages.SUCCESS);

   --  Create new empty markdown file with selected name
   elsif Argument (Number => 1) = "createfile" then
      if Argument_Count < 2 then
         Messages.Show_Message (Text => "Please specify name of file to create.");
         return;
      end if;

      if Ada.Strings.Fixed.Index
          (Source  => Argument (Number => 2),
           Pattern => Containing_Directory (Name => Current_Directory)) = 1
      then
         Work_Directory := To_Unbounded_String (Argument (Number => 2));
      else
         Work_Directory := To_Unbounded_String
               (Current_Directory & Dir_Separator & Argument (Number => 2));
      end if;

      if Extension (Name => To_String (Work_Directory)) /= "md" then
         Work_Directory :=
           Work_Directory & To_Unbounded_String (".md");
      end if;

      if Ada.Directories.Exists (Name => To_String (Work_Directory)) then
         Put_Line
           (Item =>
              "Can't create file """ & To_String (Work_Directory) &
              """. File with that name exists.");
         return;
      end if;

      Create_Path
        (New_Directory =>
           Containing_Directory (Name => To_String (Work_Directory)));
      Pages.Create_Empty_File (File_Name => To_String (Work_Directory));

      Messages.Show_Message
        (Text =>
           "Empty file """ & To_String (Work_Directory) &
           """ was created.",
         Message_Type => Messages.SUCCESS);

   --  Unknown command entered
   else
      Messages.Show_Message (Text => "Unknown command '"
                                     & Argument (Number => 1) & "'");
      Show_Help;
   end if;

exception
   when An_Exception : Invalid_Config_Data =>
      Messages.Show_Message
        (Text =>
           "Invalid data in site config file ""site.cfg"". Invalid line:""" &
            Ada.Exceptions.Exception_Message (X => An_Exception) & """");

   when AWS.Net.Socket_Error =>
      Messages.Show_Message
        (Text =>
           "Can't start program in server mode. Probably another program is "
           & "using this same port, or you have still connected old instance of "
           & "the program in your browser. Please close whole browser and try "
           & "run the program again. If problem will persist, try to change "
           & "port for the server in the site configuration.");

   when Occurrence : others =>
      Monitors.Save_Exception_Info (Occurrence, "Environment_Task");

end Yass;
