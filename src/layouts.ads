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

-- ****h* Yass/Layouts
-- FUNCTION
-- Provide code for manipulate site layouts
-- SOURCE
package Layouts is
-- ****

   -- ****f* Layouts/Layouts.CreateLayout
   -- FUNCTION
   -- Create default site layouts in directory with full path DirectoryName
   -- PARAMETERS
   -- DirectoryName - Full path to the directory where default layout will be
   --                 created
   -- SOURCE
   procedure CreateLayout(DirectoryName: String);
   -- ****

   -- ****f* Layouts/Layouts.CreateDirectoryLayout
   -- FUNCTION
   -- Create default layout for directory listing for web server in layouts
   -- directory with full path DirectoryName
   -- PARAMETERS
   -- DirectoryName - Full path to the directory where layout will be created
   -- SOURCE
   procedure CreateDirectoryLayout(DirectoryName: String);
   -- ****

end Layouts;
