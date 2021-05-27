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

with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Config; use Config;

package body Layouts is

   procedure CreateLayout(DirectoryName: String) is
      LayoutFile: File_Type;
   begin
      Create
        (LayoutFile, Append_File,
         DirectoryName & Dir_Separator & "_layouts" & Dir_Separator &
         "default.html");
      Put_Line(LayoutFile, "<!DOCTYPE html>");
      Put_Line(LayoutFile, "<html lang=""{%Language%}"">");
      Put_Line(LayoutFile, "<head>");
      Put_Line(LayoutFile, "<meta charset=""UTF-8"">");
      Put_Line
        (LayoutFile, "<link rel=""canonical"" href=""{%canonicallink%}"" />");
      Put_Line(LayoutFile, "@@IF@@ {%author%} /= """"");
      Put_Line(LayoutFile, "<meta name=""author"" content=""{%author%}"">");
      Put_Line(LayoutFile, "@@END_IF@@");
      Put_Line(LayoutFile, "@@IF@@ {%AtomLink%} /= """"");
      Put_Line(LayoutFile, "{%AtomLink%}");
      Put_Line(LayoutFile, "@@END_IF@@");
      Put_Line(LayoutFile, "<title>{%Name%}</title>");
      Put_Line(LayoutFile, "</head>");
      Put_Line(LayoutFile, "<body>");
      Put_Line(LayoutFile, "{%Content%}");
      Put_Line(LayoutFile, "</body>");
      Put_Line(LayoutFile, "</html>");
      Close(LayoutFile);
   end CreateLayout;

   procedure CreateDirectoryLayout(DirectoryName: String) is
      LayoutFile: File_Type;
   begin
      Create
        (LayoutFile, Append_File,
         DirectoryName & Dir_Separator &
         To_String(YassConfig.LayoutsDirectory) & Dir_Separator &
         "directory.html");
      Put_Line(LayoutFile, "<!DOCTYPE html>");
      Put_Line(LayoutFile, "<html>");
      Put_Line(LayoutFile, "<head>");
      Put_Line(LayoutFile, "<meta charset=""UTF-8"">");
      Put_Line(LayoutFile, "</head>");
      Put_Line(LayoutFile, "<body>");
      Put_Line(LayoutFile, "@@TABLE@@");
      Put_Line(LayoutFile, "<a href=""{%NAME_V%}"">{%NAME_V%}</a><br />");
      Put_Line(LayoutFile, "@@END_TABLE@@");
      Put_Line(LayoutFile, "</body>");
      Put_Line(LayoutFile, "</html>");
      Close(LayoutFile);
   end CreateDirectoryLayout;

end Layouts;
