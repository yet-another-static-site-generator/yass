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

package Pages is

   GenerateSiteException: exception; -- Raised when generating site was interrupted

   procedure CreatePage
     (FileName,
      Directory: String); -- Create page from file with full path FileName in directory with full path Directory
   procedure CopyFile
     (FileName,
      Directory: String); -- Copy file with full path FileName to proper location in site output directory with full path Directory
   procedure CreateEmptyFile
     (FileName: String); -- Create empty markdown file with default tags and with full path FileName
   function GetLayoutName
     (FileName: String)
     return String; -- Get name of layout used in the page with full path FileName

end Pages;
