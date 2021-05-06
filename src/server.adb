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

   HTTPServer: AWS.Server.HTTP;

   task body MonitorSite is
      SiteRebuild: Boolean;
      PageTags: Tags_Container.Map := Tags_Container.Empty_Map;
      PageTableTags: TableTags_Container.Map := TableTags_Container.Empty_Map;
      -- Monitor directory with full path Name for changes and update the site if needed
      procedure MonitorDirectory(Name: String) is
         -- Process file with full path Item: create html pages from markdown files or copy any other file if they was updated since last check.
         procedure ProcessFiles(Item: Directory_Entry_Type) is
            SiteFileName: Unbounded_String :=
              YassConfig.OutputDirectory & Dir_Separator &
              To_Unbounded_String(Simple_Name(Item));
         begin
            if YassConfig.ExcludedFiles.Find_Index(Simple_Name(Item)) /=
              Excluded_Container.No_Index or
              not Ada.Directories.Exists(Full_Name(Item)) then
               return;
            end if;
            if Containing_Directory(Full_Name(Item)) /=
              To_String(SiteDirectory) then
               SiteFileName :=
                 YassConfig.OutputDirectory &
                 Slice
                   (To_Unbounded_String(Full_Name(Item)),
                    Length(SiteDirectory) + 1, Full_Name(Item)'Length);
            end if;
            if Extension(Simple_Name(Item)) = "md" then
               SiteFileName :=
                 To_Unbounded_String
                   (Compose
                      (Containing_Directory(To_String(SiteFileName)),
                       Ada.Directories.Base_Name(To_String(SiteFileName)),
                       "html"));
            end if;
            if not Ada.Directories.Exists(To_String(SiteFileName)) then
               Set("YASSFILE", Full_Name(Item));
               if Extension(Simple_Name(Item)) = "md" then
                  CreatePage(Full_Name(Item), Name);
               else
                  CopyFile(Full_Name(Item), Name);
               end if;
               Put_Line
                 ("[" &
                  Ada.Calendar.Formatting.Image
                    (Date => Clock, Time_Zone => UTC_Time_Offset) &
                  "] " & "File: " & To_String(SiteFileName) & " was added.");
               SiteRebuild := True;
            elsif Extension(Simple_Name(Item)) = "md" then
               if Modification_Time(Full_Name(Item)) >
                 Modification_Time(To_String(SiteFileName)) or
                 Modification_Time(GetLayoutName(Full_Name(Item))) >
                   Modification_Time(To_String(SiteFileName)) then
                  Set("YASSFILE", Full_Name(Item));
                  CreatePage(Full_Name(Item), Name);
                  Put_Line
                    ("[" &
                     Ada.Calendar.Formatting.Image
                       (Date => Clock, Time_Zone => UTC_Time_Offset) &
                     "] " & "File: " & To_String(SiteFileName) &
                     " was updated.");
                  SiteRebuild := True;
               end if;
            elsif Modification_Time(Full_Name(Item)) >
              Modification_Time(To_String(SiteFileName)) then
               Set("YASSFILE", Full_Name(Item));
               CopyFile(Full_Name(Item), Name);
               Put_Line
                 ("[" &
                  Ada.Calendar.Formatting.Image
                    (Date => Clock, Time_Zone => UTC_Time_Offset) &
                  "] " & "File: " & To_String(SiteFileName) & " was updated.");
               SiteRebuild := True;
            end if;
         end ProcessFiles;
         -- Go recursive with directory with full path Item.
         procedure ProcessDirectories(Item: Directory_Entry_Type) is
         begin
            if YassConfig.ExcludedFiles.Find_Index(Simple_Name(Item)) =
              Excluded_Container.No_Index and
              Ada.Directories.Exists(Full_Name(Item)) then
               MonitorDirectory(Full_Name(Item));
            end if;
         exception
            when Ada.Directories.Name_Error =>
               null;
         end ProcessDirectories;
      begin
         Search
           (Name, "", (Directory => False, others => True),
            ProcessFiles'Access);
         Search
           (Name, "", (Directory => True, others => False),
            ProcessDirectories'Access);
      exception
         when GenerateSiteException =>
            ShowMessage
              ("[" &
               Ada.Calendar.Formatting.Image
                 (Date => Clock, Time_Zone => UTC_Time_Offset) &
               "] " & "Site rebuilding has been interrupted.");
            if YassConfig.StopServerOnError then
               if YassConfig.ServerEnabled then
                  ShutdownServer;
                  ShowMessage("done.", Success);
               end if;
               ShowMessage
                 ("Stopping monitoring site changes...done.", Success);
               OS_Exit(0);
            end if;
      end MonitorDirectory;
   begin
      select
         accept Start;
         -- Load the program modules with 'start' hook
         LoadModules("start", PageTags, PageTableTags);
         -- Load data from exisiting sitemap or create new set of data or nothing if sitemap generation is disabled
         StartSitemap;
         -- Load data from existing atom feed or create new set of data or nothing if atom feed generation is disabled
         Start_Atom_Feed;
         loop
            SiteRebuild := False;
            -- Monitor the site project directory for changes
            MonitorDirectory(To_String(SiteDirectory));
            if SiteRebuild then
               -- Save atom feed to file or nothing if atom feed generation is disabled
               Save_Atom_Feed;
               -- Save sitemap to file or nothing if sitemap generation is disabled
               SaveSitemap;
               -- Load the program modules with 'end' hook
               LoadModules("end", PageTags, PageTableTags);
               Put_Line
                 ("[" &
                  Ada.Calendar.Formatting.Image
                    (Date => Clock, Time_Zone => UTC_Time_Offset) &
                  "] " & "Site was rebuild.");
            end if;
            -- Wait before next check
            delay YassConfig.MonitorInterval;
         end loop;
      or
         terminate;
      end select;
   end MonitorSite;

   task body MonitorConfig is
      ConfigLastModified: Time;
   begin
      select
         accept Start;
         loop
            ConfigLastModified :=
              Modification_Time
                (To_String(SiteDirectory) & Dir_Separator & "site.cfg");
            -- Wait before next check
            delay YassConfig.MonitorConfigInterval;
            -- Update configuration if needed
            if ConfigLastModified /=
              Modification_Time
                (To_String(SiteDirectory) & Dir_Separator & "site.cfg") then
               Put_Line
                 ("Site configuration was changed, reconfiguring the project.");
               ParseConfig(To_String(SiteDirectory));
               ShutdownServer;
               ShowMessage("done", Messages.Success);
               if YassConfig.ServerEnabled then
                  StartServer;
               end if;
            end if;
         end loop;
      or
         terminate;
      end select;
   end MonitorConfig;

   function Callback(Request: AWS.Status.Data) return AWS.Response.Data is
      URI: constant String := AWS.Status.URI(Request);
   begin
      -- Show directory listing if requested
      if Kind(To_String(YassConfig.OutputDirectory) & URI) = Directory then
         return AWS.Response.Build
             ("text/html",
              Browse
                (To_String(YassConfig.OutputDirectory) & URI,
                 To_String(YassConfig.LayoutsDirectory) & Dir_Separator &
                 "directory.html",
                 Request));
      end if;
      -- Show selected page if requested
      return AWS.Services.Page_Server.Callback(Request);
   end Callback;

   procedure StartServer is
   begin
      AWS.Server.Start
        (HTTPServer, "YASS static page server", Port => YassConfig.ServerPort,
         Callback => Callback'Access, Max_Connection => 5);
      Put_Line
        ("Server was started. Web address: http://localhost:" &
         Positive'Image(YassConfig.ServerPort)
           (Positive'Image(YassConfig.ServerPort)'First + 1 ..
                Positive'Image(YassConfig.ServerPort)'Length) &
         "/index.html Press ""Q"" for quit.");
   end StartServer;

   procedure ShutdownServer is
   begin
      Put("Shutting down server...");
      AWS.Server.Shutdown(HTTPServer);
   end ShutdownServer;

end Server;
