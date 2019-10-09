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

-- ****h* Yass/Server
-- FUNCTION
-- Provide code for running simple HTTP server
-- SOURCE
package Server is
-- ****

   -- ****a* Server/MonitorSite
   -- FUNCTION
   -- Task for monitor changes in the site files and regenerate them
   -- SOURCE
   task MonitorSite is
      entry Start;
   end MonitorSite;
   -- ****

   -- ****a* Server/MonitorConfig
   -- FUNCTION
   -- Task for monitor changes in the site config file and reload config if
   -- needed
   -- SOURCE
   task MonitorConfig is
      entry Start;
   end MonitorConfig;
   -- ****

   -- ****f* Server/StartServer
   -- FUNCTION
   -- Start the web server
   -- SOURCE
   procedure StartServer;
   -- ****

   -- ****f* Server/ShutdownServer
   -- FUNCTION
   -- Shutdown the web server
   -- SOURCE
   procedure ShutdownServer;
   -- ****

end Server;
