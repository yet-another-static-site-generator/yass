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
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Calendar; use Ada.Calendar;

package AtomFeed is

   type FeedEntry is -- Data structure for Atom entries
   record
      Id: Unbounded_String; -- Url of entry
      EntryTitle: Unbounded_String; -- Title of entry
      Updated: Time; -- Update time of entry
   end record;
   package FeedEntry_Container is new Vectors(Positive, FeedEntry);

   function To_Time
     (Date: String) return Time; -- Convert Atom time to Ada format
   procedure StartAtomFeed; -- Create new or load existing Atom feed for the site
   procedure AddPageToFeed(FileName: String;
      Entries: in out FeedEntry_Container
        .Vector); -- Add selected page to the site Atom feed
   procedure SaveAtomFeed; -- Save Atom feed to file

end AtomFeed;
