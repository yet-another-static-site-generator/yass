-- Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Text_IO; use Ada.Text_IO;

package body Messages is

   procedure ShowMessage(Text: String; MType: Messages_Types := Error) is
   begin
      -- Change text color in terminal if needed
      case MType is
         when Error =>
            Put(ESC & "[31m");
         when Success =>
            Put(ESC & "[32m");
         when Normal =>
            null;
      end case;
      Put(Text);
      -- Reset text color in terminal if needed
      if MType /= Normal then
         Put(ESC & "[0m");
      end if;
      New_Line;
   end ShowMessage;

end Messages;
