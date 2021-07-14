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

   -- ****f* Layouts/Layouts.Create_Layout
   -- FUNCTION
   -- Create default site layouts in directory with full path DirectoryName
   -- PARAMETERS
   -- Directory_Name - Full path to the directory where default layout will be
   --                 created
   -- SOURCE
   procedure Create_Layout(Directory_Name: String) with
      Pre => Directory_Name'Length > 0,
      Test_Case => (Name => "Test_Create_Layout", Mode => Nominal);
   -- ****

   -- ****f* Layouts/Layouts.Create_Directory_Layout
   -- FUNCTION
   -- Create default layout for directory listing for web server in layouts
   -- directory with full path DirectoryName
   -- PARAMETERS
   -- Directory_Name - Full path to the directory where layout will be created
   -- SOURCE
   procedure Create_Directory_Layout(Directory_Name: String);
   -- ****

end Layouts;
