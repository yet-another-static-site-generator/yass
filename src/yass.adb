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
with Ada.Environment_Variables; use Ada.Environment_Variables;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with AWS.Server;
with AWS.Services.Page_Server;
with Config; use Config;
with Layouts; use Layouts;
with Pages; use Pages;
with Server; use Server;
with Modules; use Modules;

procedure YASS is
   Version: constant String := "0.5";
   BaseDirectory: Unbounded_String;

   function BuildSite(DirectoryName: String) return Boolean is
      procedure Build(Name: String) is
         procedure ProcessFiles(Item: Directory_Entry_Type) is
         begin
            if YassConfig.ExcludedFiles.Find_Index(Simple_Name(Item)) /=
              Excluded_Container.No_Index then
               return;
            end if;
            Set("YASSFILE", Full_Name(Item));
            LoadModules("pre");
            if Extension(Simple_Name(Item)) = "md" then
               CreatePage(Full_Name(Item), Name);
            else
               CopyFile(Full_Name(Item), Name);
            end if;
            LoadModules("post");
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
      LoadModules("start");
      Build(DirectoryName);
      LoadModules("end");
      return True;
   exception
      when GenerateSiteException =>
         return False;
   end BuildSite;

   function ValidArguments(Message: String; Exist: Boolean) return Boolean is
   begin
      if Argument_Count < 2 then
         Put_Line("Please specify directory name " & Message);
         return False;
      end if;
      if Ada.Directories.Exists
          (Current_Directory & Dir_Separator & Argument(2)) =
        Exist then
         if not Exist then
            Put_Line
              ("Directory with that name not exists, please specify existing site directory.");
         else
            Put_Line
              ("Directory with that name exists, please specify another.");
         end if;
         return False;
      end if;
      if not Exist and
        not Ada.Directories.Exists
          (Current_Directory & Dir_Separator & Argument(2) & Dir_Separator &
           "site.cfg") then
         Put_Line
           ("Selected directory don't have file ""site.cfg"". Please specify proper directory.");
         return False;
      end if;
      return True;
   end ValidArguments;

begin
   BaseDirectory := To_Unbounded_String(Current_Directory);
   if Ada.Environment_Variables.Exists("YASSDIR") then
      Set_Directory(Value("YASSDIR"));
   end if;
   if Argument_Count < 1 or else Argument(1) = "help" then
      Put_Line("Possible actions:");
      Put_Line("help - show this screen and exit");
      Put_Line("version - show the program version and exit");
      Put_Line("license - show short info about the program license");
      Put_Line("readme - show content of README file");
      Put_Line("create [name] - create new site in ""name"" directory");
      Put_Line("build [name] - build site in ""name"" directory");
      Put_Line
        ("server [name] - start simple HTTP server in ""name"" directory and auto rebuild site if needed.");
   elsif Argument(1) = "version" then
      Put_Line("Version: " & Version);
   elsif Argument(1) = "license" then
      Put_Line("Copyright (C) 2019 Bartek thindil Jasicki");
      New_Line;
      Put_Line
        ("This program is free software: you can redistribute it and/or modify");
      Put_Line
        ("it under the terms of the GNU General Public License as published by");
      Put_Line
        ("the Free Software Foundation, either version 3 of the License, or");
      Put_Line("(at your option) any later version.");
      New_Line;
      Put_Line
        ("This program is distributed in the hope that it will be useful,");
      Put_Line
        ("but WITHOUT ANY WARRANTY; without even the implied warranty of");
      Put_Line
        ("MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the");
      Put_Line("GNU General Public License for more details.");
      New_Line;
      Put_Line
        ("You should have received a copy of the GNU General Public License");
      Put_Line
        ("along with this program.  If not, see <https://www.gnu.org/licenses/>.");
   elsif Argument(1) = "readme" then
      declare
         ReadmeName: Unbounded_String;
         ReadmeFile: File_Type;
      begin
         if Ada.Environment_Variables.Exists(("APPDIR")) then
            ReadmeName :=
              To_Unbounded_String(Value("APPDIR") & "/usr/share/README.md");
         else
            ReadmeName :=
              BaseDirectory & To_Unbounded_String(Dir_Separator & "README.md");
         end if;
         if not Ada.Directories.Exists(To_String(ReadmeName)) then
            Put_Line("Can't find file " & To_String(ReadmeName));
            return;
         end if;
         Open(ReadmeFile, In_File, To_String(ReadmeName));
         while not End_Of_File(ReadmeFile) loop
            Put_Line(Get_Line(ReadmeFile));
         end loop;
         Close(ReadmeFile);
      end;
   elsif Argument(1) = "create" then
      if not ValidArguments("where new page will be created.", True) then
         return;
      end if;
      declare
         Paths: constant array(Positive range <>) of Unbounded_String :=
           (To_Unbounded_String("_layouts"), To_Unbounded_String("_output"),
            To_Unbounded_String("_modules" & Dir_Separator & "start"),
            To_Unbounded_String("_modules" & Dir_Separator & "pre"),
            To_Unbounded_String("_modules" & Dir_Separator & "post"),
            To_Unbounded_String("_modules" & Dir_Separator & "end"));
      begin
         for I in Paths'Range loop
            Create_Path
              (Current_Directory & Dir_Separator & Argument(2) &
               Dir_Separator & To_String(Paths(I)));
         end loop;
      end;
      CreateConfig(Current_Directory & Dir_Separator & Argument(2));
      CreateLayout(Current_Directory & Dir_Separator & Argument(2));
      CreateEmptyIndexFile(Current_Directory & Dir_Separator & Argument(2));
      Put_Line
        ("New page in directory """ & Argument(2) & """ was created. Edit """ &
         Argument(2) & Dir_Separator &
         "site.cfg"" file to set data for your new site.");
   elsif Argument(1) = "build" then
      if not ValidArguments("from where page will be created.", False) then
         return;
      end if;
      ParseConfig(Current_Directory & Dir_Separator & Argument(2));
      if BuildSite(Current_Directory & Dir_Separator & Argument(2)) then
         Put_Line("Site was build.");
      else
         Put_Line("Site building has been interrupted.");
      end if;
   elsif Argument(1) = "server" then
      if not ValidArguments("from where site will be served.", False) then
         return;
      end if;
      declare
         HTTPServer: AWS.Server.HTTP;
      begin
         ParseConfig(Current_Directory & Dir_Separator & Argument(2));
         Set_Directory
           (Current_Directory & Dir_Separator & Argument(2) & Dir_Separator &
            To_String(YassConfig.OutputDirectory));
         if YassConfig.ServerEnabled then
            AWS.Server.Start
              (HTTPServer, "YASS static page server",
               Port => YassConfig.ServerPort,
               Callback => AWS.Services.Page_Server.Callback'Access,
               Max_Connection => 5);
            Put_Line
              ("Server was started. Web address: http://localhost:8888/index.html Press ""Q"" for quit.");
         else
            Put_Line("Started monitoring site changes. Press ""Q"" for quit.");
         end if;
         MonitorSite.Start;
         AWS.Server.Wait(AWS.Server.Q_Key_Pressed);
         if YassConfig.ServerEnabled then
            Put("Shutting down server...");
            AWS.Server.Shutdown(HTTPServer);
         else
            Put("Stopping monitoring site changes...");
         end if;
         abort MonitorSite;
         Put_Line("done.");
      end;
   else
      Put_Line
        ("Unknown command. Please enter ""help"" as argument for program to get full list of available commands.");
   end if;
exception
   when An_Exception : InvalidConfigData =>
      Put_Line
        ("Invalid data in site config file ""site.cfg"". Invalid line:""" &
         Exception_Message(An_Exception) & """");
   when An_Exception : others =>
      declare
         ErrorFile: File_Type;
         ErrorText: Unbounded_String;
      begin
         if Ada.Directories.Exists("error.log") then
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
           ("Oops, something bad happen and program crashed. Please, remember what you done before crash and report this problem at https://github.com/thindil/yass/issues (or if you prefer, on mail thindil@laeran.pl) and attach (if possible) file 'error.log' (should be in this same directory).");
      end;
end YASS;
