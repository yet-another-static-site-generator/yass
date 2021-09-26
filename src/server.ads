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

-- ****h* Yass/Server
-- FUNCTION
-- Provide code for running simple HTTP server
-- SOURCE
package Server is
-- ****

   -- ****a* Server/Server.Monitor_Site
   -- FUNCTION
   -- Task for monitor changes in the site files and regenerate them
   -- SOURCE
   task Monitor_Site is
      entry Start;
   end Monitor_Site;
   -- ****

   -- ****a* Server/Server.Monitor_Config
   -- FUNCTION
   -- Task for monitor changes in the site config file and reload config if
   -- needed
   -- SOURCE
   task Monitor_Config is
      entry Start;
   end Monitor_Config;
   -- ****

   -- ****f* Server/Server.Start_Server
   -- FUNCTION
   -- Start the web server
   -- SOURCE
   procedure Start_Server;
   -- ****

   -- ****f* Server/Server.Shutdown_Server
   -- FUNCTION
   -- Shutdown the web server
   -- SOURCE
   procedure Shutdown_Server;
   -- ****

end Server;
