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

-- ****h* Yass/AtomFeed
-- FUNCTION
-- Provide code to create Atom feed
-- SOURCE
package AtomFeed is
-- ****

   -- ****t* AtomFeed/FeedEntry
   -- FUNCTION
   -- Data structure for Atom entries
   -- PARAMETERS
   -- Id          - Url of entry
   -- EntryTitle  - Title of entry
   -- Updated     - Update time of entry
   -- AuthorName  - Name of author of entry
   -- AuthorEmail - Email of author of entry
   -- Summary     - Short summary of entry
   -- Content     - Content of entry
   -- SOURCE
   type FeedEntry is record
      Id: Unbounded_String;
      EntryTitle: Unbounded_String;
      Updated: Time;
      AuthorName: Unbounded_String;
      AuthorEmail: Unbounded_String;
      Summary: Unbounded_String;
      Content: Unbounded_String;
   end record;
   -- ****

   -- ****t* AtomFeed/FeedEntry_Container
   -- FUNCTION
   -- Used to store Atom feed entries
   -- SOURCE
   package FeedEntry_Container is new Vectors(Positive, FeedEntry);
   -- ****

   -- ****f* AtomFeed/To_Time
   -- FUNCTION
   -- Convert HTTP date to Ada format
   -- PARAMETERS
   -- Date - HTTP date to convert
   -- RESULT
   -- Converted HTTP date to Ada Time
   -- SOURCE
   function To_Time(Date: String) return Time;
   -- ****

   -- ****f* AtomFeed/To_HTTP_Date
   -- FUNCTION
   -- Convert Ada Time to HTTP date format
   -- PARAMETERS
   -- Date - Ada Time to convert
   -- RESULT
   -- Converted Ada Time to HTTP date format
   -- SOURCE
   function To_HTTP_Date(Date: Time) return String;
   -- ****

   -- ****f* AtomFeed/StartAtomFeed
   -- FUNCTION
   -- Load existing Atom feed for the site
   -- SOURCE
   procedure StartAtomFeed;
   -- ****

   -- ****f* AtomFeed/AddPageToFeed
   -- FUNCTION
   -- Add page with full path FileName and it extracted Atom entries Entries to the site Atom feed
   -- PARAMETERS
   -- FileName - File name of the page to add
   -- Entries  - List of Atom feed entries
   -- SOURCE
   procedure AddPageToFeed
     (FileName: String; Entries: in out FeedEntry_Container.Vector);
   -- ****

   -- ****f* AtomFeed/SaveAtomFeed
   -- FUNCTION
   -- Save Atom feed to file
   -- SOURCE
   procedure SaveAtomFeed;
   -- ****

end AtomFeed;
