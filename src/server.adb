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

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Directory_Operations;

with AWS.Config.Set;
with AWS.Response;
with AWS.Services.Page_Server;
with AWS.Services.Directory;
with AWS.Server;
with AWS.Status;

with Config; use Config;

package body Server is

   --## rule off GLOBAL_REFERENCES
   -- ****iv* Server/Server.Http_Server
   -- FUNCTION
   -- Instance of Http server which will be serving the project's files
   -- SOURCE
   Http_Server : AWS.Server.HTTP;
   -- ****
   --## rule on GLOBAL_REFERENCES

   -- ****f** Server/Server.Callback
   -- FUNCTION
   -- Handle callbacks from HTTP server.
   -- SOURCE
   function Callback (Request : AWS.Status.Data)
                      return AWS.Response.Data;
   -- ****

   --------------
   -- Callback --
   --------------

   function Callback (Request : AWS.Status.Data) return AWS.Response.Data
   is
      use Ada.Directories;
      use Ada.Strings.Unbounded;
      use GNAT.Directory_Operations;
      use AWS.Services.Directory;

      Uri : constant String := AWS.Status.URI (D => Request);
   begin
      -- Show directory listing if requested
      if Kind
          (Name => To_String (Yass_Conf.Output_Directory) & Uri) =
        Directory
      then
         return
           AWS.Response.Build
             (Content_Type => "text/html",
              Message_Body =>
                Browse
                  (Directory_Name =>
                     To_String (Yass_Conf.Output_Directory) & Uri,
                   Template_Filename =>
                     To_String (Yass_Conf.Layouts_Directory) &
                     Dir_Separator & "directory.html",
                   Request => Request));
      end if;
      -- Show selected page if requested
      return AWS.Services.Page_Server.Callback (Request => Request);
   end Callback;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server
   is
      use Ada.Text_IO;
      use AWS.Config;

      Server_Config : Object := Default_Config;
   begin
      Set.Server_Name    (Server_Config, "YASS static page server");
      Set.Server_Port    (Server_Config, Yass_Conf.Server_Port);
      Set.Max_Connection (Server_Config, 5);
      Set.Reuse_Address  (Server_Config, True);

      AWS.Server.Start (Web_Server => Http_Server,
                        Callback   => Callback'Access,
                        Config     => Server_Config);

      Put_Line ("Server was started.");
      Put_Line ("Web address: http://localhost:" &
                Ada.Strings.Fixed.Trim (Yass_Conf.Server_Port'Image,
                                        Side => Ada.Strings.Left) &
                "/index.html");
      Put_Line ("Press ""Q"" for quit.");
   end Start_Server;

   ---------------------
   -- Shutdown_Server --
   ---------------------

   procedure Shutdown_Server
   is
      use Ada.Text_IO;
   begin
      Put ("Shutting down server...");
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      AWS.Server.Shutdown (Web_Server => Http_Server);
      --## rule on DIRECTLY_ACCESSED_GLOBALS
   end Shutdown_Server;

end Server;
