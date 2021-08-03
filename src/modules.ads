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

with Config; use Config; --## rule line off REDUCEABLE_SCOPE

-- ****h* Yass/Modules
-- FUNCTION
-- Provide code for operating the program modules
-- SOURCE
package Modules is
-- ****

   -- ****f* Modules/Modules.Load_Modules
   -- FUNCTION
   -- Load all modules for selected state: start, pre, post, end. PageTags and
   -- PageTableTags will be empty in all states except pre and post for
   -- markdown files.
   -- PARAMETERS
   -- State           - State of the program in which module are loaded
   -- Page_Tags       - All current processed page tags with their contents
   -- Page_Table_Tags - All current processed page table tags with their contents
   -- SOURCE
   procedure Load_Modules
     (State: String; Page_Tags: in out Tags_Container.Map;
      Page_Table_Tags: in out TableTags_Container.Map) with
      Pre => State'Length > 0,
      Test_Case => (Name => "Test_Load_Modules", Mode => Nominal);
   -- ****

end Modules;
