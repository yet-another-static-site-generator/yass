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

with AWS.Status;
with AWS.Response;

package Server is

   task MonitorSite is -- Task for monitor changes in the site files and regenerate them
      entry Start;
   end MonitorSite;

   task MonitorConfig is -- Task for monitor changes in the site config file and reload config if needed
      entry Start;
   end MonitorConfig;

   function Callback
     (Request: AWS.Status.Data) return AWS.Response
     .Data; -- Handle server responses for HTTP request Request
   procedure StartServer; -- Start the web server
   procedure ShutdownServer; -- Shutdown the web server

end Server;
