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

-- ****h* Yass/Sitemaps
-- FUNCTION
-- Provide code for create sitemap
-- SOURCE
package Sitemaps is
-- ****

   -- ****f* Sitemaps/Sitemaps.StartSitemap
   -- FUNCTION
   -- Create new or load existing sitemap for the site
   -- SOURCE
   procedure StartSitemap with
      Test_Case => (Name => "Test_Start_Sitemap", Mode => Robustness);
   -- ****

   -- ****f* Sitemaps/Sitemaps.AddPageToSitemap
   -- FUNCTION
   -- Add selected page with full path FileName, it ChangeFrequency and
   -- PagePriority to the sitemap
   -- PARAMETERS
   -- FileName        - Full path to the page which will be added to the
   --                   sitemap
   -- ChangeFrequency - How often selected page will change
   -- PagePriority    - Priority for the page
   -- SOURCE
   procedure AddPageToSitemap
     (FileName, ChangeFrequency, PagePriority: String) with
      Pre => FileName'Length > 0,
      Test_Case => (Name => "Test_Add_Page_To_Sitemap", Mode => Robustness);
   -- ****

   -- ****f* Sitemaps/Sitemaps.SaveSitemap
   -- FUNCTION
   -- Save site map to file
   -- SOURCE
   procedure SaveSitemap with
      Test_Case => (Name => "Test_Save_Sitemap", Mode => Robustness);
   -- ****

end Sitemaps;
