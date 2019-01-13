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

with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.UTF_Encoding.Wide_Strings;
use Ada.Strings.UTF_Encoding.Wide_Strings;

package body Layouts is

   procedure CreateLayout(DirectoryName: String) is
      LayoutFile: File_Type;
   begin
      Create
        (LayoutFile, Append_File, DirectoryName & "/_layouts/default.html");
      Put_Line(LayoutFile, "<!DOCTYPE html>");
      Put_Line(LayoutFile, "<html>");
      Put_Line(LayoutFile, "<head>");
      Put_Line(LayoutFile, "<meta charset=""UTF-8"">");
      Put_Line(LayoutFile, "<title>{%Name%}</title>");
      Put_Line(LayoutFile, "</head>");
      Put_Line(LayoutFile, "<body>");
      Put_Line(LayoutFile, "{%Contents%}");
      Put_Line(LayoutFile, "</body>");
      Put_Line(LayoutFile, "</html>");
      Close(LayoutFile);
   end CreateLayout;

   function LoadLayout(FileName: String) return Unbounded_String is
      Layout: Unbounded_String;
      LayoutFile: File_Type;
   begin
      Open(LayoutFile, In_File, FileName);
      while not End_Of_File(LayoutFile) loop
         Append(Layout, Encode(Get_Line(LayoutFile)));
         Append(Layout, LF);
      end loop;
      Close(LayoutFile);
      return Layout;
   end LoadLayout;

end Layouts;
