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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Config; use Config;
with Pages; use Pages;

package body Server is

   task body MonitorSite is
      SiteRebuild: Boolean;
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
               Dir_Separator & To_String(YassConfig.OutputDirectory));
            if Extension(Simple_Name(Item)) = "md" then
               SiteFileName :=
                 To_Unbounded_String
                   (Compose
                      (Containing_Directory(To_String(SiteFileName)),
                       Ada.Directories.Base_Name(To_String(SiteFileName)),
                       "html"));
            end if;
            if not Exists(To_String(SiteFileName)) then
               if Extension(Simple_Name(Item)) = "md" then
                  CreatePage(Full_Name(Item), Name);
               else
                  CopyFile(Full_Name(Item), Name);
               end if;
               Put_Line("File: " & To_String(SiteFileName) & " was added.");
               SiteRebuild := True;
            elsif Extension(Simple_Name(Item)) = "md" then
               if Modification_Time(Full_Name(Item)) >
                 Modification_Time(To_String(SiteFileName)) or
                 Modification_Time(GetLayoutName(Full_Name(Item))) >
                   Modification_Time(To_String(SiteFileName)) then
                  CreatePage(Full_Name(Item), Name);
                  Put_Line
                    ("File: " & To_String(SiteFileName) & " was updated.");
                  SiteRebuild := True;
               end if;
            elsif Modification_Time(Full_Name(Item)) >
              Modification_Time(To_String(SiteFileName)) then
               CopyFile(Full_Name(Item), Name);
               Put_Line("File: " & To_String(SiteFileName) & " was updated.");
               SiteRebuild := True;
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
      exception
         when GenerateSiteException =>
            Put_Line("Site rebuilding has been interrupted.");
      end MonitorDirectory;
   begin
      select
         accept Start;
         loop
            SiteRebuild := False;
            MonitorDirectory(To_String(SiteDirectory));
            if SiteRebuild then
               Put_Line("Site was rebuild.");
            end if;
            delay 5.0;
         end loop;
      or
         terminate;
      end select;
   end MonitorSite;

end Server;
