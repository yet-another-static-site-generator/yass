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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with AWS.Services.Page_Server;
with AWS.Services.Directory; use AWS.Services.Directory;
with AWS.Server;
with AWS.Status;
with AWS.Response;
with Config; use Config;
with Pages; use Pages;
with Modules; use Modules;
with Sitemaps; use Sitemaps;
with AtomFeed; use AtomFeed;
with Messages; use Messages;

package body Server is

   Http_Server: AWS.Server.HTTP;

   task body Monitor_Site is
      Site_Rebuild: Boolean := False;
      Page_Tags: Tags_Container.Map := Tags_Container.Empty_Map;
      Page_Table_Tags: TableTags_Container.Map :=
        TableTags_Container.Empty_Map;
      -- Monitor directory with full path Name for changes and update the site if needed
      procedure Monitor_Directory(Name: String) is
         -- Process file with full path Item: create html pages from markdown
         -- files or copy any other file if they was updated since last check.
         procedure Process_Files(Item: Directory_Entry_Type) is
            Site_File_Name: Unbounded_String :=
              Yass_Config.Output_Directory & Dir_Separator &
              To_Unbounded_String
                (Source => Simple_Name(Directory_Entry => Item));
         begin
            if Yass_Config.Excluded_Files.Find_Index
                (Item => Simple_Name(Directory_Entry => Item)) /=
              Excluded_Container.No_Index or
              not Ada.Directories.Exists
                (Name => Full_Name(Directory_Entry => Item)) then
               return;
            end if;
            if Containing_Directory
                (Name => Full_Name(Directory_Entry => Item)) /=
              To_String(Source => Site_Directory) then
               Site_File_Name :=
                 Yass_Config.Output_Directory &
                 Slice
                   (Source =>
                      To_Unbounded_String
                        (Source => Full_Name(Directory_Entry => Item)),
                    Low => Length(Source => Site_Directory) + 1,
                    High => Full_Name(Directory_Entry => Item)'Length);
            end if;
            if Extension(Simple_Name(Item)) = "md" then
               Site_File_Name :=
                 To_Unbounded_String
                   (Compose
                      (Containing_Directory(To_String(Site_File_Name)),
                       Ada.Directories.Base_Name(To_String(Site_File_Name)),
                       "html"));
            end if;
            if not Ada.Directories.Exists(To_String(Site_File_Name)) then
               Set("YASSFILE", Full_Name(Item));
               if Extension(Simple_Name(Item)) = "md" then
                  Create_Page(Full_Name(Item), Name);
               else
                  Pages.Copy_File(Full_Name(Item), Name);
               end if;
               Put_Line
                 ("[" &
                  Ada.Calendar.Formatting.Image
                    (Date => Clock, Time_Zone => UTC_Time_Offset) &
                  "] " & "File: " & To_String(Site_File_Name) & " was added.");
               Site_Rebuild := True;
            elsif Extension(Simple_Name(Item)) = "md" then
               if Modification_Time(Full_Name(Item)) >
                 Modification_Time(To_String(Site_File_Name)) or
                 Modification_Time(Get_Layout_Name(Full_Name(Item))) >
                   Modification_Time(To_String(Site_File_Name)) then
                  Set("YASSFILE", Full_Name(Item));
                  Create_Page(Full_Name(Item), Name);
                  Put_Line
                    ("[" &
                     Ada.Calendar.Formatting.Image
                       (Date => Clock, Time_Zone => UTC_Time_Offset) &
                     "] " & "File: " & To_String(Site_File_Name) &
                     " was updated.");
                  Site_Rebuild := True;
               end if;
            elsif Modification_Time(Full_Name(Item)) >
              Modification_Time(To_String(Site_File_Name)) then
               Set("YASSFILE", Full_Name(Item));
               Pages.Copy_File(Full_Name(Item), Name);
               Put_Line
                 ("[" &
                  Ada.Calendar.Formatting.Image
                    (Date => Clock, Time_Zone => UTC_Time_Offset) &
                  "] " & "File: " & To_String(Site_File_Name) &
                  " was updated.");
               Site_Rebuild := True;
            end if;
         end Process_Files;
         -- Go recursive with directory with full path Item.
         procedure ProcessDirectories(Item: Directory_Entry_Type) is
         begin
            if Yass_Config.Excluded_Files.Find_Index(Simple_Name(Item)) =
              Excluded_Container.No_Index and
              Ada.Directories.Exists(Full_Name(Item)) then
               Monitor_Directory(Full_Name(Item));
            end if;
         exception
            when Ada.Directories.Name_Error =>
               null;
         end ProcessDirectories;
      begin
         Search
           (Name, "", (Directory => False, others => True),
            Process_Files'Access);
         Search
           (Name, "", (Directory => True, others => False),
            ProcessDirectories'Access);
      exception
         when Generate_Site_Exception =>
            Show_Message
              ("[" &
               Ada.Calendar.Formatting.Image
                 (Date => Clock, Time_Zone => UTC_Time_Offset) &
               "] " & "Site rebuilding has been interrupted.");
            if Yass_Config.Stop_Server_On_Error then
               if Yass_Config.Server_Enabled then
                  Shutdown_Server;
                  Show_Message("done.", SUCCESS);
               end if;
               Show_Message
                 ("Stopping monitoring site changes...done.", SUCCESS);
               OS_Exit(0);
            end if;
      end Monitor_Directory;
   begin
      select
         accept Start;
         -- Load the program modules with 'start' hook
         Load_Modules("start", Page_Tags, Page_Table_Tags);
         -- Load data from exisiting sitemap or create new set of data or nothing if sitemap generation is disabled
         Start_Sitemap;
         -- Load data from existing atom feed or create new set of data or nothing if atom feed generation is disabled
         Start_Atom_Feed;
         loop
            Site_Rebuild := False;
            -- Monitor the site project directory for changes
            Monitor_Directory(To_String(Site_Directory));
            if Site_Rebuild then
               -- Save atom feed to file or nothing if atom feed generation is disabled
               Save_Atom_Feed;
               -- Save sitemap to file or nothing if sitemap generation is disabled
               Save_Sitemap;
               -- Load the program modules with 'end' hook
               Load_Modules("end", Page_Tags, Page_Table_Tags);
               Put_Line
                 ("[" &
                  Ada.Calendar.Formatting.Image
                    (Date => Clock, Time_Zone => UTC_Time_Offset) &
                  "] " & "Site was rebuild.");
            end if;
            -- Wait before next check
            delay Yass_Config.Monitor_Interval;
         end loop;
      or
         terminate;
      end select;
   end Monitor_Site;

   task body Monitor_Config is
      ConfigLastModified: Time;
   begin
      select
         accept Start;
         loop
            ConfigLastModified :=
              Modification_Time
                (To_String(Site_Directory) & Dir_Separator & "site.cfg");
            -- Wait before next check
            delay Yass_Config.Monitor_Config_Interval;
            -- Update configuration if needed
            if ConfigLastModified /=
              Modification_Time
                (To_String(Site_Directory) & Dir_Separator & "site.cfg") then
               Put_Line
                 ("Site configuration was changed, reconfiguring the project.");
               Parse_Config(To_String(Site_Directory));
               Shutdown_Server;
               Show_Message("done", Messages.SUCCESS);
               if Yass_Config.Server_Enabled then
                  Start_Server;
               end if;
            end if;
         end loop;
      or
         terminate;
      end select;
   end Monitor_Config;

   function Callback(Request: AWS.Status.Data) return AWS.Response.Data is
      URI: constant String := AWS.Status.URI(Request);
   begin
      -- Show directory listing if requested
      if Kind(To_String(Yass_Config.Output_Directory) & URI) = Directory then
         return
           AWS.Response.Build
             ("text/html",
              Browse
                (To_String(Yass_Config.Output_Directory) & URI,
                 To_String(Yass_Config.Layouts_Directory) & Dir_Separator &
                 "directory.html",
                 Request));
      end if;
      -- Show selected page if requested
      return AWS.Services.Page_Server.Callback(Request);
   end Callback;

   procedure Start_Server is
   begin
      AWS.Server.Start
        (Http_Server, "YASS static page server",
         Port => Yass_Config.Server_Port, Callback => Callback'Access,
         Max_Connection => 5);
      Put_Line
        ("Server was started. Web address: http://localhost:" &
         Positive'Image(Yass_Config.Server_Port)
           (Positive'Image(Yass_Config.Server_Port)'First + 1 ..
                Positive'Image(Yass_Config.Server_Port)'Length) &
         "/index.html Press ""Q"" for quit.");
   end Start_Server;

   procedure Shutdown_Server is
   begin
      Put("Shutting down server...");
      AWS.Server.Shutdown(Http_Server);
   end Shutdown_Server;

end Server;
