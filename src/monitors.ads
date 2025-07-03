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

with Ada.Exceptions;

-- ****h* Yass/Monitors
-- SOURCE
package Monitors
-- FUNCTION
-- Provide code for running simple HTTP server
-- ****
is
   -- ****a* Monitors/Monitors.Monitor_Site
   -- SOURCE
   task Monitor_Site is
      entry Start;
      entry Stop;
   end Monitor_Site;
   -- FUNCTION
   -- Task for monitor changes in the site files and regenerate them
   -- ****

   -- ****a* Monitors/Monitor.Monitor_Config
   -- SOURCE
   task Monitor_Config is
      entry Start;
      entry Stop;
   end Monitor_Config;
   -- FUNCTION
   -- Task for monitor changes in the site config file and reload config if
   -- needed
   -- ****

   -- ****f* Monitors/Monitor.Save_Exception_Info
   -- SOURCE
   procedure Save_Exception_Info (Occurrence : Ada.Exceptions.Exception_Occurrence;
                                  Task_Name  : String);
   -- FUNCTION
   -- Save exception traceback to error.log
   -- PARAMETERS
   -- Occurrence - Exception occurence information
   -- Task_Name  - Identifier for task.
   -- ****

end Monitors;
