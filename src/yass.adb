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
with Config; use Config;
with Layouts; use Layouts;
with Pages; use Pages;

procedure YASS is
   Version: constant String := "0.1";
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
      Put_Line("create [name] - create new page in name directory");
   elsif Argument(1) = "version" then
      Put_Line("Version: " & Version);
   elsif Argument(1) = "create" then
      if Argument_Count < 2 then
         Put_Line
           ("Please specify directory name where new page will be created.");
         return;
      end if;
      if Exists(Argument(2)) then
         Put_Line("Directory with that name exists, please specify another.");
         return;
      end if;
      Create_Path(Argument(2) & "/_layouts");
      Create_Path(Argument(2) & "/_output");
      CreateConfig(Argument(2));
      CreateLayout(Argument(2));
      Put_Line
        ("New page in directory """ & Argument(2) & """ was created. Edit """ &
         Argument(2) & "/site.cfg"" file to set data for your new site.");
   elsif Argument(1) = "build" then
      if Argument_Count < 2 then
         Put_Line
           ("Please specify directory name from where page will be created.");
         return;
      end if;
      if not Exists(Argument(2)) then
         Put_Line
           ("Directory with that name not exists, please specify existing directory.");
         return;
      end if;
      if not Exists(Argument(2) & "/site.cfg") then
         Put_Line
           ("Selected directory don't have file ""site.cfg"". Please specify proper directory.");
         return;
      end if;
      ParseConfig(Argument(2));
      declare
         Entries: Search_Type;
         FoundEntry: Directory_Entry_Type;
         InvalidNames: constant array(Positive range <>) of Unbounded_String :=
           (To_Unbounded_String("."), To_Unbounded_String(".."),
            YassConfig.LayoutsDirectory, YassConfig.OutputDirectory,
            To_Unbounded_String("site.cfg"));
         ValidEntry: Boolean;
      begin
         Start_Search(Entries, Argument(2), "");
         while More_Entries(Entries) loop
            Get_Next_Entry(Entries, FoundEntry);
            ValidEntry := True;
            for I in InvalidNames'Range loop
               if InvalidNames(I) =
                 To_Unbounded_String(Simple_Name(FoundEntry)) then
                  ValidEntry := False;
                  exit;
               end if;
            end loop;
            if ValidEntry then
               if Extension(Simple_Name(FoundEntry)) = "md" then
                  CreatePage(Full_Name(FoundEntry), Argument(2));
               end if;
            end if;
         end loop;
         End_Search(Entries);
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
