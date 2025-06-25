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

-- ****h* Yass/CMark
-- FUNCTION
-- Provide wrapper for cmark library
-- SOURCE
package CMark is
-- ****

   -- ****f* CMark/CMark. Markdown_To_HTML
   -- FUNCTION
   -- Convert markdown into HTML
   -- PARAMETERS
   -- Text - Unicode-encoded Ada String
   --  HTML_Enabled - True if passing existing HTML through.
   --  RETURN
   --  String: the markdown source now encoded as HTML
   -- SOURCE
   function Markdown_To_HTML (Text         : in String;
                              HTML_Enabled : in Boolean)
                              return String;
   -- ****

end CMark;
