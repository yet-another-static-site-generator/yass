-- Copyright (c) 2019-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

-- ****h* Yass/Messages
-- FUNCTION
-- Provides code to show the program messages to the user
-- SOURCE
package Messages is
-- ****

   -- ****t* Messages/Messages.Messages_Types
   -- FUNCTION
   -- Types of messages: Normal, Error or Success
   -- SOURCE
   type Messages_Types is (Normal, Error, Success) with
      Default_Value => Error;
   -- ****

   -- ****f* Messages/Messages.ShowMessage
   -- FUNCTION
   -- Show selected message to the user.
   -- PARAMETERS
   -- Text  - Text to show to the user
   -- MType - Type of message. Default is Error
   -- SOURCE
   procedure ShowMessage(Text: String; MType: Messages_Types := Error) with
      Pre => Text'Length > 0,
      Test_Case => (Name => "Test_Show_Message", Mode => Nominal);
   -- ****

end Messages;
