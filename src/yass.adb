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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with AWS.Server;
with AWS.Services.Page_Server;
with Config; use Config;
with Layouts; use Layouts;
with Pages; use Pages;

procedure YASS is
   Version: constant String := "0.1";

   procedure BuildSite(DirectoryName: String) is
      procedure Build(Name: String) is
         procedure ProcessFiles(Item: Directory_Entry_Type) is
         begin
            if YassConfig.ExcludedFiles.Find_Index(Simple_Name(Item)) /=
              Excluded_Container.No_Index then
               return;
            end if;
            if Extension(Simple_Name(Item)) = "md" then
               CreatePage(Full_Name(Item), Name);
            else
               CopyFile(Full_Name(Item), Name);
            end if;
         end ProcessFiles;
         procedure ProcessDirectories(Item: Directory_Entry_Type) is
         begin
            if YassConfig.ExcludedFiles.Find_Index(Simple_Name(Item)) =
              Excluded_Container.No_Index then
               Build(Full_Name(Item));
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
      end Build;
   begin
      Build(DirectoryName);
   end BuildSite;

   task MonitorSite is
      entry Start;
   end MonitorSite;

   task body MonitorSite is
      NeedRebuildSite: Boolean;
      procedure MonitorDirectory(Name: String) is
         procedure ProcessFiles(Item: Directory_Entry_Type) is
            SiteFileName: Unbounded_String :=
              To_Unbounded_String(Full_Name(Item));
         begin
            if YassConfig.ExcludedFiles.Find_Index(Simple_Name(Item)) /=
              Excluded_Container.No_Index then
               return;
            end if;
            Insert
              (SiteFileName, Length(SiteDirectory) + 1,
               "/" & To_String(YassConfig.OutputDirectory));
            if Extension(Simple_Name(Item)) = "md" then
               SiteFileName :=
                 To_Unbounded_String
                   (Compose
                      (Containing_Directory(To_String(SiteFileName)),
                       Base_Name(To_String(SiteFileName)), "html"));
            end if;
            if Modification_Time(Full_Name(Item)) >
              Modification_Time(To_String(SiteFileName)) then
               NeedRebuildSite := True;
            end if;
         end ProcessFiles;
         procedure ProcessDirectories(Item: Directory_Entry_Type) is
         begin
            if YassConfig.ExcludedFiles.Find_Index(Simple_Name(Item)) =
              Excluded_Container.No_Index then
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
      end MonitorDirectory;
   begin
      select
         accept Start;
         loop
            NeedRebuildSite := False;
            MonitorDirectory(To_String(SiteDirectory));
            if NeedRebuildSite then
               BuildSite(To_String(SiteDirectory));
               Put_Line("Site was rebuild.");
            end if;
            delay 5.0;
         end loop;
      or
         terminate;
      end select;
   end MonitorSite;

begin
   if Argument_Count < 1 then
      Put_Line
        ("Please specify what you want to do. To see possible actions, type ""yass help""");
      return;
   end if;
   if Argument(1) = "help" then
      Put_Line("Possible actions:");
      Put_Line("help - show this screen and exit");
      Put_Line("version - show program version and exit");
      Put_Line("create [name] - create new site in ""name"" directory");
      Put_Line("build [name] - build site in ""name"" directory");
      Put_Line
        ("server [name] - start simple HTTP server in ""name"" directory and auto rebuild site if needed.");
   elsif Argument(1) = "version" then
      Put_Line("Version: " & Version);
   elsif Argument(1) = "create" then
      if Argument_Count < 2 then
         Put_Line
           ("Please specify directory name where new page will be created.");
         return;
      end if;
      if Exists(Current_Directory & "/" & Argument(2)) then
         Put_Line("Directory with that name exists, please specify another.");
         return;
      end if;
      Create_Path(Current_Directory & "/" & Argument(2) & "/_layouts");
      Create_Path(Current_Directory & "/" & Argument(2) & "/_output");
      CreateConfig(Current_Directory & "/" & Argument(2));
      CreateLayout(Current_Directory & "/" & Argument(2));
      Put_Line
        ("New page in directory """ & Argument(2) & """ was created. Edit """ &
         Argument(2) & "/site.cfg"" file to set data for your new site.");
   elsif Argument(1) = "build" then
      if Argument_Count < 2 then
         Put_Line
           ("Please specify directory name from where page will be created.");
         return;
      end if;
      if not Exists(Current_Directory & "/" & Argument(2)) then
         Put_Line
           ("Directory with that name not exists, please specify existing directory.");
         return;
      end if;
      if not Exists(Current_Directory & "/" & Argument(2) & "/site.cfg") then
         Put_Line
           ("Selected directory don't have file ""site.cfg"". Please specify proper directory.");
         return;
      end if;
      ParseConfig(Current_Directory & "/" & Argument(2));
      BuildSite(Current_Directory & "/" & Argument(2));
      Put_Line("Site was build.");
   elsif Argument(1) = "server" then
      if Argument_Count < 2 then
         Put_Line
           ("Please specify directory name from where site will be served.");
         return;
      end if;
      if not Exists(Current_Directory & "/" & Argument(2)) then
         Put_Line
           ("Directory with that name not exists, please specify existing directory.");
         return;
      end if;
      if not Exists(Current_Directory & "/" & Argument(2) & "/site.cfg") then
         Put_Line
           ("Selected directory don't have file ""site.cfg"". Please specify proper directory.");
         return;
      end if;
      declare
         HTTPServer: AWS.Server.HTTP;
      begin
         ParseConfig(Current_Directory & "/" & Argument(2));
         Set_Directory
           (Current_Directory & "/" & Argument(2) & "/" &
            To_String(YassConfig.OutputDirectory));
         AWS.Server.Start
           (HTTPServer, "YASS static page server", Port => 8888,
            Callback => AWS.Services.Page_Server.Callback'Access,
            Max_Connection => 5);
         Put_Line
           ("Server was started. Web address: http://localhost:8888/index.html Press ""CTRL+C"" for quit.");
         MonitorSite.Start;
         AWS.Server.Wait(AWS.Server.Forever);
         AWS.Server.Shutdown(HTTPServer);
      end;
   end if;
exception
   when An_Exception : others =>
      declare
         ErrorFile: File_Type;
         ErrorText: Unbounded_String;
      begin
         if Exists("error.log") then
            Open(ErrorFile, Append_File, "error.log");
         else
            Create(ErrorFile, Append_File, "error.log");
         end if;
         Append(ErrorText, Ada.Calendar.Formatting.Image(Clock));
         Append(ErrorText, LF);
         Append(ErrorText, Version);
         Append(ErrorText, LF);
         Append(ErrorText, "Exception: " & Exception_Name(An_Exception));
         Append(ErrorText, LF);
         Append(ErrorText, "Message: " & Exception_Message(An_Exception));
         Append(ErrorText, LF);
         Append
           (ErrorText, "-------------------------------------------------");
         Append(ErrorText, LF);
         Append(ErrorText, Symbolic_Traceback(An_Exception));
         Append(ErrorText, LF);
         Append
           (ErrorText, "-------------------------------------------------");
         Put_Line(ErrorFile, To_String(ErrorText));
         Close(ErrorFile);
         Put_Line
           ("Oops, something bad happen and program crashed. Please, remember what you done before crash and report this problem at https://github.com/thindil/yass/issues (or if you prefer, on mail thindil@laeran.pl) and attach (if possible) file 'error.log' (should be in this same directory where this file is).");
      end;
end YASS;
