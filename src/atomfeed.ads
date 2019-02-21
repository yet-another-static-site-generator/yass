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

with Ada.Containers.Indefinite_Vectors; use Ada.Containers;

package AtomFeed is

   package String_Container is new Indefinite_Vectors(Positive, String);

   procedure StartAtomFeed; -- Create new or load existing Atom feed for the site
   procedure AddPageToFeed(FileName: String;
      Titles: String_Container
        .Vector); -- Add selected page to the site Atom feed
   procedure SaveAtomFeed; -- Save Atom feed to file

end AtomFeed;
