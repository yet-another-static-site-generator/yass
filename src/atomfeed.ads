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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Calendar; use Ada.Calendar;

-- ****h* Yass/AtomFeed
-- FUNCTION
-- Provide code to create Atom feed
-- SOURCE
package AtomFeed is
-- ****

   -- ****t* AtomFeed/Feed_Entry
   -- FUNCTION
   -- Data structure for Atom entries
   -- PARAMETERS
   -- Id           - Url of entry
   -- Entry_Title  - Title of entry
   -- Updated      - Update time of entry
   -- Author_Name  - Name of author of entry
   -- Author_Email - Email of author of entry
   -- Summary      - Short summary of entry
   -- Content      - Content of entry
   -- SOURCE
   type Feed_Entry is record
      Id: Unbounded_String;
      Entry_Title: Unbounded_String;
      Updated: Time;
      Author_Name: Unbounded_String;
      Author_Email: Unbounded_String;
      Summary: Unbounded_String;
      Content: Unbounded_String;
   end record;
   -- ****

   -- ****d* AtomFeed/Empty_Feed_Entry
   -- FUNCTION
   -- Empty Atom feed entry
   -- SOURCE
   Empty_Feed_Entry: constant Feed_Entry := (others => <>);
   -- ****

   -- ****t* AtomFeed/FeedEntry_Container
   -- FUNCTION
   -- Used to store Atom feed entries
   -- SOURCE
   package FeedEntry_Container is new Vectors(Index_Type => Positive,
      Element_Type => Feed_Entry);
   -- ****

   -- ****f* AtomFeed/To_Time
   -- FUNCTION
   -- Convert HTTP date to Ada format
   -- PARAMETERS
   -- Date - HTTP date to convert
   -- RESULT
   -- Converted HTTP date to Ada Time
   -- SOURCE
   function To_Time(Date: String) return Time with
      Test_Case => (Name => "Test_To_Date", Mode => Nominal);
   -- ****

   -- ****f* AtomFeed/To_HTTP_Date
   -- FUNCTION
   -- Convert Ada Time to HTTP date format
   -- PARAMETERS
   -- Date - Ada Time to convert
   -- RESULT
   -- Converted Ada Time to HTTP date format
   -- SOURCE
   function To_HTTP_Date
     (Date: Time) return String with --## rule line off NAMING_CONVENTION
      Test_Case => (Name => "Test_To_HTTP_Date", Mode => Nominal);
   -- ****

   -- ****f* AtomFeed/Start_Atom_Feed
   -- FUNCTION
   -- Load existing Atom feed for the site
   -- SOURCE
   procedure Start_Atom_Feed;
   -- ****

   -- ****f* AtomFeed/Add_Page_To_Feed
   -- FUNCTION
   -- Add page with full path File_Name and it extracted Atom entries Entries to the site Atom feed
   -- PARAMETERS
   -- File_Name - File name of the page to add
   -- Entries  - List of Atom feed entries
   -- SOURCE
   procedure Add_Page_To_Feed
     (File_Name: String; Entries: in out FeedEntry_Container.Vector);
   -- ****

   -- ****f* AtomFeed/Save_Atom_Feed
   -- FUNCTION
   -- Save Atom feed to file
   -- SOURCE
   procedure Save_Atom_Feed;
   -- ****

end AtomFeed;
