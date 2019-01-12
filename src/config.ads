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
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Config is

   type ParserConfig is -- Data structure for setting for parser
   record
      OutputDirectory: Unbounded_String; -- Path to directory with generated site
      LayoutsDirectory: Unbounded_String; -- Path to directory where site layouts are
   end record;
   YassConfig: ParserConfig; -- Settings for parser
   package Tags_Container is new Ada.Containers.Indefinite_Hashed_Maps(String,
      String, Ada.Strings.Hash, "=");
   SiteTags: Tags_Container.Map; -- Site tags (like title, author, etc)
   SiteDirectory: Unbounded_String; -- Directory where site files are

   procedure CreateConfig
     (DirectoryName: String); -- Create default config in selected directory
   procedure ParseConfig
     (DirectoryName: String); -- Parse config file and set all settings and tags for site

end Config;
