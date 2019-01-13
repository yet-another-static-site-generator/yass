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

with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Directories; use Ada.Directories;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Layouts; use Layouts;
with Config; use Config;

package body Pages is

   subtype size_t is unsigned_long;

   function cmark_markdown_to_html(text: Interfaces.C.Strings.chars_ptr;
      len: size_t; options: int) return Interfaces.C.Strings.chars_ptr;
   pragma Import(C, cmark_markdown_to_html, "cmark_markdown_to_html");

   procedure CreatePage(FileName, Directory: String) is
      Layout, Data, Contents: Unbounded_String;
      PageFile: File_Type;
      StartIndex: Natural;
      PageTags: Tags_Container.Map;
   begin
      Open(PageFile, In_File, FileName);
      while not End_Of_File(PageFile) loop
         -- FIXME: Crash on links
         Data := To_Unbounded_String(Encode(Get_Line(PageFile)));
         if Length(Data) > 2 then
            if Slice(Data, 1, 3) = "-- " then
               if Index(Data, "layout:", 1) > 0 then
                  Data := Unbounded_Slice(Data, 12, Length(Data));
                  Layout :=
                    LoadLayout
                      (To_String(SiteDirectory) & "/" &
                       To_String(YassConfig.LayoutsDirectory) & "/" &
                       To_String(Data) & ".html");
               else
                  StartIndex := Index(Data, ":", 1);
                  Tags_Container.Include
                    (PageTags, Slice(Data, 4, StartIndex - 1),
                     Slice(Data, StartIndex + 2, Length(Data)));
               end if;
            else
               Append(Contents, Data);
               Append(Contents, LF);
            end if;
         else
            Append(Contents, Data);
            Append(Contents, LF);
         end if;
      end loop;
      Close(PageFile);
      Contents :=
        To_Unbounded_String
          (Value
             (cmark_markdown_to_html
                (New_String(To_String(Contents)), size_t(Length(Contents)),
                 0)));
      StartIndex := Index(Layout, "{%Contents%}", 1);
      Replace_Slice(Layout, StartIndex, StartIndex + 12, To_String(Contents));
      for I in SiteTags.Iterate loop
         loop
            StartIndex := Index(Layout, "{%" & Tags_Container.Key(I) & "%}");
            exit when StartIndex = 0;
            Replace_Slice
              (Layout, StartIndex,
               StartIndex + 3 + Tags_Container.Key(I)'Length, SiteTags(I));
         end loop;
      end loop;
      for I in PageTags.Iterate loop
         loop
            StartIndex := Index(Layout, "{%" & Tags_Container.Key(I) & "%}");
            exit when StartIndex = 0;
            Replace_Slice
              (Layout, StartIndex,
               StartIndex + 3 + Tags_Container.Key(I)'Length, PageTags(I));
         end loop;
      end loop;
      declare
         OutputDirectory: constant Unbounded_String :=
           SiteDirectory & "/" & YassConfig.OutputDirectory &
           Delete(To_Unbounded_String(Directory), 1, Length(SiteDirectory));
      begin
         Create_Path(To_String(OutputDirectory));
         Create
           (PageFile, Append_File,
            To_String(OutputDirectory) & "/" & Base_Name(FileName) & ".html");
         Put(PageFile, Decode(To_String(Layout)));
         Close(PageFile);
      end;
   end CreatePage;

   procedure CopyFile(FileName, Directory: String) is
      OutputDirectory: constant Unbounded_String :=
        SiteDirectory & "/" & YassConfig.OutputDirectory &
        Delete(To_Unbounded_String(Directory), 1, Length(SiteDirectory));
   begin
      Create_Path(To_String(OutputDirectory));
      Copy_File
        (FileName, To_String(OutputDirectory) & "/" & Simple_Name(FileName));
   end CopyFile;

end Pages;
