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
with GNAT.String_Split; use GNAT.String_Split;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Config is

   procedure CreateConfig(DirectoryName: String) is
      ConfigFile: File_Type;
   begin
      Create
        (ConfigFile, Append_File, DirectoryName & Dir_Separator & "site.cfg");
      Put_Line(ConfigFile, "LayoutsDirectory = _layouts");
      Put_Line(ConfigFile, "OutputDirectory = _output");
      Put_Line(ConfigFile, "# Site tags");
      Put_Line(ConfigFile, "Name = New Site");
      Close(ConfigFile);
   end CreateConfig;

   procedure ParseConfig(DirectoryName: String) is
      ConfigFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex: Natural;
      Tokens: Slice_Set;
   begin
      Open(ConfigFile, In_File, DirectoryName & "/site.cfg");
      while not End_Of_File(ConfigFile) loop
         RawData := To_Unbounded_String(Encode(Get_Line(ConfigFile)));
         if Length(RawData) > 0 and Element(RawData, 1) /= '#' then
            EqualIndex := Index(RawData, "=");
            FieldName := Head(RawData, EqualIndex - 2);
            Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
            if FieldName = To_Unbounded_String("LayoutsDirectory") then
               YassConfig.LayoutsDirectory := Value;
            elsif FieldName = To_Unbounded_String("OutputDirectory") then
               YassConfig.OutputDirectory := Value;
            elsif FieldName = To_Unbounded_String("ExcludedFiles") then
               Create(Tokens, To_String(Value), ",");
               for I in 1 .. Slice_Count(Tokens) loop
                  YassConfig.ExcludedFiles.Append(Slice(Tokens, I));
               end loop;
            else
               Tags_Container.Include
                 (SiteTags, To_String(FieldName), To_String(Value));
            end if;
         end if;
      end loop;
      Close(ConfigFile);
      YassConfig.ExcludedFiles.Append(".");
      YassConfig.ExcludedFiles.Append("..");
      YassConfig.ExcludedFiles.Append("site.cfg");
      YassConfig.ExcludedFiles.Append(To_String(YassConfig.LayoutsDirectory));
      YassConfig.ExcludedFiles.Append(To_String(YassConfig.OutputDirectory));
      SiteDirectory := To_Unbounded_String(DirectoryName);
   end ParseConfig;

end Config;
