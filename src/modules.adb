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
with GNAT.Expect; use GNAT.Expect;
with Config; use Config;

package body Modules is

   procedure LoadModules(State: String) is
      procedure RunModule(Item: Directory_Entry_Type) is
         Module: Process_Descriptor;
         Finished: Boolean := False;
         Result: Expect_Match;
         Text, TagName: Unbounded_String;
      begin
         if not Is_Executable_File(Full_Name(Item)) then
            return;
         end if;
         Non_Blocking_Spawn(Module, Full_Name(Item), Argument_String_To_List("").all);
         while not Finished loop
            Expect(Module, Result, ".+", 1_000);
            case Result is
               when Expect_Timeout =>
                  Finished := True;
               when 1 =>
                  Text := To_Unbounded_String(Expect_Out_Match(Module));
                  if Slice(Text, 1, 6) = "gettag" then
                     TagName := Unbounded_Slice(Text, 8, Length(Text));
                  elsif Slice(Text, 1, 7) = "edittag" then
                     TagName := Unbounded_Slice(Text, 9, Length(Text));
                  else
                     Put_Line(To_String(Text));
                  end if;
               when others =>
                  null;
            end case;
         end loop;
         Close(Module);
      exception
         when Invalid_Process =>
            Put_Line("Module " & Full_Name(Item) & " failed to execute.");
         when Process_Died =>
            null;
      end RunModule;
   begin
      if not Exists
          (To_String(YassConfig.ModulesDirectory) & Dir_Separator & State) then
         return;
      end if;
      Search
        (To_String(YassConfig.ModulesDirectory) & Dir_Separator & State, "",
         (Directory => False, others => True), RunModule'Access);
   end LoadModules;

end Modules;
