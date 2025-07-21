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

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;

with AtomFeed;
with Config; use Config;
with Messages; use Messages;
with Modules;
with Pages;
with Server;
with Sitemaps;

package body Monitors is

   Error_File_Name : constant String := "error.log";

   -- ****f* Monitors/Monitors.Log
   -- SOURCE
   procedure Log (Message : String);
   -- FUNCTION
   -- Log message
   -- PARAMETERS
   -- Message - Put Message with log
   -- ****

   -- ****f* Monitors/Monitors.To_Relative
   -- SOURCE
   function To_Relative (Full_Name : String;
                         Base_Name : String)
                         return String;
   -- FUNCTION
   -- Get relative path to file
   -- PARAMETERS
   -- Full_Name - Full name of file
   -- Base_Name - Name to subtract from Full_Name
   -- RETURNS
   -- Path to file relative to Base_Name
   -- ****

   ---------
   -- Log --
   ---------

   procedure Log (Message : String)
   is
      use Ada.Calendar;

      Time_Now   : constant Time   := Clock;
      Image_Time : constant String :=
         Formatting.Image (Date      => Time_Now,
                           Time_Zone => Time_Zones.UTC_Time_Offset);
   begin
      Put_Line ("[" & Image_Time & "] " & Message);
   end Log;

   -----------------
   -- To_Relative --
   -----------------

   function To_Relative (Full_Name : String;
                         Base_Name : String)
                         return String
   is
      use Ada.Strings.Fixed;
   begin
      pragma Assert (Head (Full_Name, Base_Name'Length) = Base_Name);
      return Tail (Full_Name, Full_Name'Length - Base_Name'Length);
   end To_Relative;

   ------------------
   -- Monitor_Site --
   ------------------

   task body Monitor_Site is
      use type Ada.Calendar.Time;
      use AtomFeed;
      use Modules;
      use Sitemaps;

      Site_Rebuild    : Boolean := False; --## rule line off GLOBAL_REFERENCES
      Page_Tags       : Tags_Container.Map := Tags_Container.Empty_Map;
      Page_Table_Tags : TableTags_Container.Map :=
        TableTags_Container.Empty_Map;

      procedure Monitor_Directory (Name : String);
      --  Monitor directory with full path Name for changes and update the site
      --  if needed

      -----------------------
      -- Monitor_Directory --
      -----------------------

      procedure Monitor_Directory (Name : String) is
         use GNAT.OS_Lib;
         use Pages;

         procedure Process_Files (Item : Directory_Entry_Type);
         --  Process file with full path Item: create html pages from markdown
         --  files or copy any other file if they was updated since last check.

         procedure Process_Directories (Item : Directory_Entry_Type);
         --  Go recursive with directory with full path Item.

         -------------------
         -- Process_Files --
         -------------------

         procedure Process_Files (Item : Directory_Entry_Type)
         is
            Site_File_Name : Unbounded_String :=
              Yass_Conf.Output_Directory & Dir_Separator &
              To_Unbounded_String
                (Source => Simple_Name (Item));
         begin
            if
              Yass_Conf.Excluded_Files.Find_Index (Simple_Name (Item)) /=
              Excluded_Container.No_Index or
              not Ada.Directories.Exists (Full_Name (Item))
            then
               return;
            end if;

            if
              Containing_Directory (Full_Name (Item)) /=
              To_String (Site_Directory)
            then
               Site_File_Name :=
                 Yass_Conf.Output_Directory &
                 Slice
                   (Source => To_Unbounded_String (Full_Name (Item)),
                    Low    => Length (Site_Directory) + 1,
                    High   => Full_Name (Item)'Length);
            end if;

            if Extension (Simple_Name (Item)) = "md" then
               Site_File_Name :=
                 To_Unbounded_String
                   (Source =>
                      Compose
                        (Containing_Directory =>
                           Containing_Directory
                             (Name => To_String (Site_File_Name)),
                         Name =>
                           Ada.Directories.Base_Name
                             (Name => To_String (Site_File_Name)),
                         Extension => "html"));
            end if;

            if not Ada.Directories.Exists (To_String (Site_File_Name)) then
               Ada.Environment_Variables.Set
                 (Name  => "YASSFILE",
                  Value => Full_Name (Item));

               if Extension (Simple_Name (Item)) = "md" then
                  Create_Page (File_Name => Full_Name (Item),
                               Directory => Name);
               else
                  Pages.Copy_File (File_Name => Full_Name (Item),
                                   Directory => Name);
               end if;

               Log ("File: " &
                    To_Relative
                      (Full_Name => To_String (Site_File_Name),
                       Base_Name => To_String (Site_Directory) & Dir_Separator) &
                    " was added.");

               Site_Rebuild := True;

            elsif Extension (Simple_Name (Item)) = "md" then
               if Get_Layout_Name (Full_Name (Item)) = "" then
                  Log ("File: " &
                       To_Relative
                         (Full_Name => Full_Name (Item),
                          Base_Name => To_String (Site_Directory) & Dir_Separator) &
                       " has no layout");
               elsif
                 Modification_Time (Full_Name (Item)) >
                 Modification_Time (To_String (Site_File_Name)) or
                 Modification_Time (Get_Layout_Name (Full_Name (Item))) >
                 Modification_Time (To_String (Site_File_Name))
               then
                  Ada.Environment_Variables.Set (Name  => "YASSFILE",
                                                 Value => Full_Name (Item));

                  Create_Page
                    (File_Name => Full_Name (Item),
                     Directory => Name);

                  Log ("File: " &
                       To_Relative
                         (Full_Name => To_String (Site_File_Name),
                          Base_Name => To_String (Site_Directory) & Dir_Separator) &
                       " was updated.");

                  Site_Rebuild := True;
               end if;
            elsif
              Modification_Time (Full_Name (Item)) >
              Modification_Time (To_String (Site_File_Name))
            then
               Ada.Environment_Variables.Set (Name  => "YASSFILE",
                                              Value => Full_Name (Item));

               Pages.Copy_File
                 (File_Name => Full_Name (Item),
                  Directory => Name);

               Log ("File: " &
                    To_Relative
                      (Full_Name => To_String (Site_File_Name),
                       Base_Name => To_String (Site_Directory) & Dir_Separator) &
                    " was updated.");

               Site_Rebuild := True;
            end if;
         end Process_Files;

         -------------------------
         -- Process_Directories --
         -------------------------

         procedure Process_Directories (Item : Directory_Entry_Type) is
         begin
            if
              Yass_Conf.Excluded_Files.Find_Index (Simple_Name (Item)) =
              Excluded_Container.No_Index and
              Ada.Directories.Exists (Full_Name (Item))
            then
               Monitor_Directory (Full_Name (Item));
            end if;
         exception
            when Ada.Directories.Name_Error =>
               null;
         end Process_Directories;

      begin
         Search
           (Directory => Name, Pattern => "",
            Filter    => (Directory => False, others => True),
            Process   => Process_Files'Access);
         Search
           (Directory => Name, Pattern => "",
            Filter    => (Directory => True, others => False),
            Process   => Process_Directories'Access);

      exception
         when Generate_Site_Exception =>

            Log ("Site rebuilding has been interrupted.");

            if Yass_Conf.Stop_Server_On_Error then
               if Yass_Conf.Server_Enabled then
                  Server.Shutdown_Server;
                  Show_Message (Text => "done.", Message_Type => SUCCESS);
               end if;
               Show_Message
                 (Text => "Stopping monitoring site changes...done.",
                  Message_Type => SUCCESS);
               OS_Exit (Status => 0);
            end if;
      end Monitor_Directory;

      Site_Monitor_Running : Boolean := True;
   begin
      select
         accept Start;
         --  Load the program modules with 'start' hook
         Load_Modules
           (State => "start", Page_Tags => Page_Tags,
            Page_Table_Tags => Page_Table_Tags);

         --  Load data from exisiting sitemap or create new set of data or
         --  nothing if sitemap generation is disabled
         Start_Sitemap;

         --  Load data from existing atom feed or create new set of data or
         --  nothing if atom feed generation is disabled
         Start_Atom_Feed;

         Monitor_Site_Loop :
         while Site_Monitor_Running loop
            Site_Rebuild := False;

            -- Monitor the site project directory for changes
            Monitor_Directory (Name => To_String (Site_Directory));

            if Site_Rebuild then

               -- Save atom feed to file or nothing if atom feed generation is disabled
               Save_Atom_Feed;

               -- Save sitemap to file or nothing if sitemap generation is disabled

               Save_Sitemap;

               -- Load the program modules with 'end' hook
               Load_Modules
                 (State => "end", Page_Tags => Page_Tags,
                  Page_Table_Tags => Page_Table_Tags);

               Log ("Site was rebuild.");

            end if;

            select
               accept Stop do
                  Site_Monitor_Running := False;
               end Stop;
            or
               --  Wait before next check
               delay Yass_Conf.Monitor_Interval;
            end select;
         end loop Monitor_Site_Loop;
      or
         terminate;
      end select;

   exception
      when Occurrence : others =>
         Save_Exception_Info (Occurrence, "Monitor_Site");

   end Monitor_Site;

   --------------------
   -- Monitor_Config --
   --------------------

   task body Monitor_Config is
      use Ada.Calendar;

      Config_Last_Modified   : Time; --## rule line off IMPROPER_INITIALIZATION
      Config_Monitor_Running : Boolean := True;
   begin
      select
         accept Start;

         Monitor_Config_Loop :
         while Config_Monitor_Running loop
            Config_Last_Modified :=
              Modification_Time
                (Name =>
                   To_String (Site_Directory) & Dir_Separator &
                   "site.cfg");
            select
               accept Stop do
                  Config_Monitor_Running := False;
               end Stop;
            or
               --  Wait before next check
               delay Yass_Conf.Monitor_Config_Interval;
            end select;

            -- Update configuration if needed
            if Config_Last_Modified /=
              Modification_Time
                (Name =>
                   To_String (Site_Directory) & Dir_Separator &
                   "site.cfg")
            then
               Put_Line
                 (Item =>
                    "Site configuration was changed, reconfiguring the project.");

               Load_Site_Config (Directory_Name => To_String (Site_Directory));

               Server.Shutdown_Server;

               Show_Message (Text => "done", Message_Type => Messages.SUCCESS);

               if Yass_Conf.Server_Enabled then
                  Server.Start_Server;
               end if;

            end if;
         end loop Monitor_Config_Loop;
      or
         terminate;
      end select;

   exception
      when Occurence : others =>
         Save_Exception_Info (Occurence, "Monitor_Config");

   end Monitor_Config;

   -------------------------
   -- Save_Exception_Info --
   -------------------------

   procedure Save_Exception_Info (Occurrence : Ada.Exceptions.Exception_Occurrence;
                                  Task_Name  : String)
   is
      use Ada.Calendar;
      use Ada.Exceptions;
      use GNAT.Traceback.Symbolic;

      Time_Now   : constant Time   := Clock;
      Image_Time : constant String :=
         Formatting.Image (Date      => Time_Now,
                           Time_Zone => Time_Zones.UTC_Time_Offset);
      Error_File : File_Type;
   begin
      if Ada.Directories.Exists (Error_File_Name) then
         Open (File => Error_File,
               Mode => Append_File,
               Name => Error_File_Name);
      else
         Create (File => Error_File,
                 Mode => Append_File,
                 Name => Error_File_Name);
      end if;

      Put_Line (Error_File, Image_Time);
      Put_Line (Error_File, "Exception from: " & Task_Name);
--      Put_Line (Error_File, Version);
      Put_Line (Error_File,
                "Exception: " & Exception_Name (Occurrence));
      Put_Line (Error_File,
                "Message: " & Exception_Message (Occurrence));
      Put_Line (Error_File,
                "-------------------------------------------------");

      if Dir_Separator = '/' then
         Put_Line (Error_File,
                   Symbolic_Traceback (Occurrence));
      else
         Put_Line (Error_File,
                   Exception_Information (Occurrence));
      end if;

      Put_Line (Error_File,
                "-------------------------------------------------");

      Close (Error_File);

      Put_Line ("Oops, something bad happened and the program crashed.");
      Put_Line ("Please, remember what you did before the crash and report this");
      Put_Line ("problem at");
      Put_Line ("");
      Put_Line ("   https://github.com/yet-another-static-site-generator/yass");
      Put_Line ("");
      Put_Line ("and attach (if possible) file 'error.log' (should be in this");
      Put_Line ("directory).");
   end Save_Exception_Info;

end Monitors;
