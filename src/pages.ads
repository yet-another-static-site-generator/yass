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

-- ****h* Yass/Pages
-- FUNCTION
-- Provide code to create pages from Markdown files
-- SOURCE
package Pages is
-- ****

   -- ****e* Pages/Pages.Generate_Site_Exception
   -- FUNCTION
   -- Raised when generating site was interrupted
   -- SOURCE
   Generate_Site_Exception : exception;
   -- ****

   -- ****f* Pages/Pages.Create_Page
   -- FUNCTION
   -- Create page from file with full path FileName in directory with full
   -- path Directory
   -- PARAMETERS
   -- File_Name - Full path to the Markdown file which will be processed
   -- Directory - Full path to the directory where newly create HTML file will
   --             be added
   -- SOURCE
   procedure Create_Page (File_Name : String;
                          Directory : String)
   with
      Pre => File_Name'Length > 0 and Directory'Length > 0,
      Test_Case => (Name => "Test_Create_Page",
                    Mode => Nominal);
   -- ****

   -- ****f* Pages/Pages.Copy_File
   -- FUNCTION
   -- Copy file with full path FileName to proper location in site output
   -- directory with full path Directory
   -- PARAMETERS
   -- File_Name - Full path to the file which will be copied
   -- Directory - Full path to the directory where file will be added
   -- SOURCE
   procedure Copy_File (File_Name : String;
                        Directory : String)
   with
      Pre => File_Name'Length > 0 and Directory'Length > 0,
      Test_Case => (Name => "Test_Copy_File",
                    Mode => Nominal);
   -- ****

   -- ****f* Pages/Pages.Create_Empty_File
   -- FUNCTION
   -- Create empty markdown file with default tags and with full path FileName
   -- PARAMETERS
   -- File_Name - Full path to the Markdown file which will be created
   -- SOURCE
   procedure Create_Empty_File (File_Name : String)
   with
      Pre => File_Name'Length > 0,
      Test_Case => (Name => "Test_Create_Empty_File",
                    Mode => Nominal);
   -- ****

   -- ****f* Pages/Pages.Get_Layout_Name
   -- FUNCTION
   -- Get name of layout used in the page with full path FileName
   -- PARAMETERS
   -- File_Name - Full path to the file which name layout will be check
   -- RESULT
   -- Name of the layout used by the file
   -- SOURCE
   function Get_Layout_Name (File_Name : String) return String
   with
      Pre => File_Name'Length > 0,
      Test_Case => (Name => "Test_Get_Layout_Name",
                    Mode => Nominal);
   -- ****

end Pages;
