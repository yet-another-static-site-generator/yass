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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Config; use Config;

package body Modules is

   procedure LoadModules(State: String) is
      procedure RunModule(Item: Directory_Entry_Type) is
         Success: Boolean;
      begin
         if not Is_Executable_File(Full_Name(Item)) then
            return;
         end if;
         Spawn(Full_Name(Item), Argument_String_To_List("").all, Success);
         if not Success then
            Put_Line("Module " & Full_Name(Item) & " failed to execute.");
         end if;
      end RunModule;
   begin
      Search
        (To_String(SiteDirectory) & Dir_Separator &
         To_String(YassConfig.ModulesDirectory) & Dir_Separator & State,
         "", (Directory => False, others => True), RunModule'Access);
   end LoadModules;

end Modules;
