--    Copyright 2019-2021 Bartek thindil Jasicki & 2025 J. Quorning
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

with Interfaces.C.Strings;

package body CMark is

   use Interfaces.C;

   subtype size_t is unsigned_long;

   ------------------------------
   -- C_CMark_Markdown_to_HTML --
   ------------------------------

   function C_CMark_Markdown_To_HTML
     (Text    : Strings.chars_ptr;
      Len     : size_t;
      Options : int)
      return Strings.chars_ptr
   with
      Import        => True,
      Convention    => C,
      External_Name => "cmark_markdown_to_html";

   ----------------------
   -- Markdown_to_HTML --
   ----------------------

   function Markdown_To_HTML (Text         : in String;
                              HTML_Enabled : in Boolean)
                              return String
   is
      C_Options : constant int :=
        (if HTML_Enabled then 16#20000# else 0);

      C_Text   :          Strings.chars_ptr := Strings.New_String (Text);
      C_Len    : constant size_t            := size_t (Text'Length);
      C_Result : constant Strings.chars_ptr :=
         C_CMark_Markdown_To_HTML (Text    => C_Text,
                                   Len     => C_Len,
                                   Options => C_Options);
   begin
      Strings.Free (C_Text);

      return Strings.Value (C_Result);
   end Markdown_To_HTML;

end CMark;
