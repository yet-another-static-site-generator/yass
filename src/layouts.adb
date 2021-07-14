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

   procedure Create_Layout(Directory_Name: String) is
      Layout_File: File_Type;
   begin
      Create
        (File => Layout_File, Mode => Append_File,
         Name =>
           Directory_Name & Dir_Separator & "_layouts" & Dir_Separator &
           "default.html");
      Put_Line(File => Layout_File, Item => "<!DOCTYPE html>");
      Put_Line(File => Layout_File, Item => "<html lang=""{%Language%}"">");
      Put_Line(File => Layout_File, Item => "<head>");
      Put_Line(File => Layout_File, Item => "<meta charset=""UTF-8"">");
      Put_Line
        (File => Layout_File,
         Item =>
           "<meta name=""viewport"" content=""width=device-width, initial-scale=1.0"">");
      Put_Line
        (File => Layout_File,
         Item => "<link rel=""canonical"" href=""{%canonicallink%}"" />");
      Put_Line(File => Layout_File, Item => "@@IF@@ {%author%} /= """"");
      Put_Line
        (File => Layout_File,
         Item => "<meta name=""author"" content=""{%author%}"">");
      Put_Line(File => Layout_File, Item => "@@END_IF@@");
      Put_Line(File => Layout_File, Item => "@@IF@@ {%description%} /= """"");
      Put_Line
        (File => Layout_File,
         Item => "<meta name=""description"" content=""{%description%}"">");
      Put_Line(File => Layout_File, Item => "@@END_IF@@");
      Put_Line(File => Layout_File, Item => "@@IF@@ {%AtomLink%} /= """"");
      Put_Line(File => Layout_File, Item => "{%AtomLink%}");
      Put_Line(File => Layout_File, Item => "@@END_IF@@");
      Put_Line(File => Layout_File, Item => "<title>{%Name%}</title>");
      Put_Line(File => Layout_File, Item => "</head>");
      Put_Line(File => Layout_File, Item => "<body>");
      Put_Line(File => Layout_File, Item => "{%Content%}");
      Put_Line(File => Layout_File, Item => "</body>");
      Put_Line(File => Layout_File, Item => "</html>");
      Close(File => Layout_File);
   end Create_Layout;

   procedure Create_Directory_Layout(Directory_Name: String) is
      Layout_File: File_Type;
   begin
      Create
        (File => Layout_File, Mode => Append_File,
         Name =>
           Directory_Name & Dir_Separator &
           To_String(Source => Yass_Config.Layouts_Directory) & Dir_Separator &
           "directory.html");
      Put_Line(File => Layout_File, Item => "<!DOCTYPE html>");
      Put_Line(File => Layout_File, Item => "<html>");
      Put_Line(File => Layout_File, Item => "<head>");
      Put_Line(File => Layout_File, Item => "<meta charset=""UTF-8"">");
      Put_Line
        (File => Layout_File,
         Item =>
           "<meta name=""viewport"" content=""width=device-width, initial-scale=1.0"">");
      Put_Line(Layout_File, "</head>");
      Put_Line(Layout_File, "<body>");
      Put_Line(Layout_File, "@@TABLE@@");
      Put_Line(Layout_File, "<a href=""{%NAME_V%}"">{%NAME_V%}</a><br />");
      Put_Line(Layout_File, "@@END_TABLE@@");
      Put_Line(Layout_File, "</body>");
      Put_Line(Layout_File, "</html>");
      Close(Layout_File);
   end Create_Directory_Layout;

end Layouts;
