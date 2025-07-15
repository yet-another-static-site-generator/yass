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

with Ada.Calendar;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Readers;
with Input_Sources.File;
with AtomFeed;
with Config; use Config;

package body Sitemaps is

   -- ****iv* Sitemaps/Sitemaps.Sitemap
   -- FUNCTION
   -- The content of the sitemap of the project
   -- SOURCE
   Sitemap: Document; --## rule line off GLOBAL_REFERENCES
   -- ****

   -- ****if* Sitemaps/Sitemaps.Get_Sitemap
   -- FUNCTION
   -- Get the project sitemap content
   -- RESULT
   -- The Document with sitemap of the current project
   -- SOURCE
   function Get_Sitemap return Document is
      -- ****
   begin
      return Sitemap;
   end Get_Sitemap;

   -- ****if* Sitemaps/Sitemaps.Set_Sitemap
   -- FUNCTION
   -- Set the project sitemap content
   -- PARAMETERS
   -- New_Sitemap - The sitemap which will be used as the project sitemap
   -- SOURCE
   procedure Set_Sitemap(New_Sitemap: Document) is
      -- ****
   begin
      Sitemap := New_Sitemap;
   end Set_Sitemap;

   -- ****iv* Sitemaps/Sitemaps.Main_Node
   -- FUNCTION
   -- The main XML node of the project's sitemap
   -- SOURCE
   Main_Node: DOM.Core.Element; --## rule line off GLOBAL_REFERENCES
   -- ****

   -- ****if* Sitemaps/Set_Main_Node
   -- FUNCTION
   -- Set the new main XML node for the sitemap of the project
   -- PARAMETERS
   -- New_Main_Node - The XML node which will be set as the new main sitemap
   -- node
   -- SOURCE
   procedure Set_Main_Node(New_Main_Node: DOM.Core.Element) is
      -- ****
   begin
      Main_Node := New_Main_Node;
   end Set_Main_Node;

   -- ****iv* Sitemaps/Sitemaps.Sitemap_File_Name
   -- FUNCTION
   -- The name of the file which contains the project's sitemap
   -- SOURCE
   Sitemap_File_Name: Unbounded_String; --## rule line off GLOBAL_REFERENCES
   -- ****

   -- ****if* Sitemaps/Get_Sitemap_File_Name
   -- FUNCTION
   -- Get the name of file which contains the project's sitemap
   -- RESULT
   -- Unbounded_String with the name of sitemap file
   -- SOURCE
   function Get_Sitemap_File_Name return Unbounded_String is
      -- ****
   begin
      return Sitemap_File_Name;
   end Get_Sitemap_File_Name;

   procedure Start_Sitemap is
      use DOM.Core.Elements;
      use DOM.Readers;
      use Input_Sources.File;

      Sitemap_File: File_Input;
      --## rule off IMPROPER_INITIALIZATION
      Reader: Tree_Reader;
      New_Sitemap: DOM_Implementation;
      Nodes_List: Node_List;
      Local_Sitemap: Document;
      Local_Main_Node: DOM.Core.Element;
      --## rule on IMPROPER_INITIALIZATION
   begin
      if not Yass_Conf.Sitemap_Enabled then
         return;
      end if;
      Sitemap_File_Name :=
        Yass_Conf.Output_Directory &
        To_Unbounded_String(Source => Dir_Separator & "sitemap.xml");
      -- Load existing sitemap data
      if Exists(Name => To_String(Source => Get_Sitemap_File_Name)) then
         Open
           (Filename => To_String(Source => Get_Sitemap_File_Name),
            Input => Sitemap_File);
         --## rule off IMPROPER_INITIALIZATION
         Parse(Parser => Reader, Input => Sitemap_File);
         Close(Input => Sitemap_File);
         Local_Sitemap := Get_Tree(Read => Reader);
         --## rule on IMPROPER_INITIALIZATION
         Nodes_List :=
           DOM.Core.Documents.Get_Elements_By_Tag_Name
             (Doc => Local_Sitemap, Tag_Name => "urlset");
         Local_Main_Node := Item(List => Nodes_List, Index => 0);
         Set_Attribute
           (Elem => Local_Main_Node, Name => "xmlns",
            Value => "http://www.sitemaps.org/schemas/sitemap/0.9");
         -- Create new sitemap data
      else
         Local_Sitemap := Create_Document(Implementation => New_Sitemap);
         Local_Main_Node :=
           Create_Element(Doc => Local_Sitemap, Tag_Name => "urlset");
         Set_Attribute
           (Elem => Local_Main_Node, Name => "xmlns",
            Value => "http://www.sitemaps.org/schemas/sitemap/0.9");
         Local_Main_Node :=
           Append_Child(N => Local_Sitemap, New_Child => Local_Main_Node);
      end if;
      Set_Sitemap(New_Sitemap => Local_Sitemap);
      Set_Main_Node(New_Main_Node => Local_Main_Node);
   end Start_Sitemap;

   procedure Add_Page_To_Sitemap
     (File_Name, Change_Frequency, Page_Priority: String) is
      use Ada.Calendar;
      use AtomFeed;

      Url: constant String :=
        To_String(Source => Yass_Conf.Base_Url) & "/" &
        Slice
          (Source => To_Unbounded_String(Source => File_Name),
           Low =>
             Length(Source => Yass_Conf.Output_Directory & Dir_Separator) +
             1,
           High => File_Name'Length);
      Urls_List: Node_List;
      Children_List: Node_List; --## rule line off IMPROPER_INITIALIZATION
      Added, Frequency_Updated, Priority_Updated: Boolean := False;
      Url_Node, Url_Data, Old_Main_Node, Remove_Frequency,
      Remove_Priority: DOM.Core.Element;
      Url_Text: Text;
      Last_Modified: constant String := To_HTTP_Date(Date => Clock);
      Local_Sitemap: constant Document := Get_Sitemap;
      Local_Main_Node: DOM.Core.Element := Main_Node;
   begin
      if not Yass_Conf.Sitemap_Enabled then
         return;
      end if;
      Urls_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Local_Sitemap, Tag_Name => "loc");
      Load_Existing_Urls_Loop :
      for I in 0 .. Length(List => Urls_List) - 1 loop
         if Node_Value
             (N => First_Child(N => Item(List => Urls_List, Index => I))) /=
           Url then
            goto End_Of_Loop;
         end if;
         -- Update sitemap entry if exists
         Url_Node := Parent_Node(N => Item(List => Urls_List, Index => I));
         Children_List := Child_Nodes(N => Url_Node);
         Update_Entry_Loop :
         for J in 0 .. Length(List => Children_List) - 1 loop
            if Node_Name(N => Item(List => Children_List, Index => J)) =
              "lastmod" then
               Url_Text :=
                 First_Child(N => Item(List => Children_List, Index => J));
               Set_Node_Value(N => Url_Text, Value => Last_Modified);
            elsif Node_Name(N => Item(List => Children_List, Index => J)) =
              "changefreq" then
               if Change_Frequency'Length > 0 then
                  Url_Text :=
                    First_Child(N => Item(List => Children_List, Index => J));
                  Set_Node_Value(N => Url_Text, Value => Change_Frequency);
               else
                  Remove_Frequency := Item(List => Children_List, Index => J);
               end if;
               Frequency_Updated := True;
            elsif Node_Name(N => Item(List => Children_List, Index => J)) =
              "priority" then
               if Page_Priority'Length > 0 then
                  Url_Text :=
                    First_Child(N => Item(List => Children_List, Index => J));
                  Set_Node_Value(N => Url_Text, Value => Page_Priority);
               else
                  Remove_Priority := Item(List => Children_List, Index => J);
               end if;
               Priority_Updated := True;
            end if;
         end loop Update_Entry_Loop;
         if Change_Frequency'Length > 0 and not Frequency_Updated then
            Url_Data :=
              Append_Child
                (N => Url_Node,
                 New_Child =>
                   Create_Element
                     (Doc => Local_Sitemap, Tag_Name => "changefreq"));
            Url_Text :=
              Append_Child
                (N => Url_Data,
                 New_Child =>
                   Create_Text_Node
                     (Doc => Local_Sitemap, Data => Change_Frequency));
         end if;
         if Page_Priority'Length > 0 and not Priority_Updated then
            Url_Data :=
              Append_Child
                (N =>
                   Create_Element
                     (Doc => Local_Sitemap, Tag_Name => "priority"),
                 New_Child => Url_Data);
            Url_Text :=
              Append_Child
                (N => Url_Data,
                 New_Child =>
                   Create_Text_Node
                     (Doc => Local_Sitemap, Data => Page_Priority));
         end if;
         if Remove_Frequency /= null then
            Url_Node :=
              Remove_Child(N => Url_Node, Old_Child => Remove_Frequency);
         end if;
         if Remove_Priority /= null then
            Url_Node :=
              Remove_Child(N => Url_Node, Old_Child => Remove_Priority);
         end if;
         Added := True;
         exit Load_Existing_Urls_Loop;
         <<End_Of_Loop>>
      end loop Load_Existing_Urls_Loop;
      -- Add new sitemap entry
      if not Added then
         Url_Node := Create_Element(Doc => Local_Sitemap, Tag_Name => "url");
         Old_Main_Node := Local_Main_Node;
         Local_Main_Node :=
           Append_Child(N => Local_Main_Node, New_Child => Url_Node);
         --## rule off ASSIGNMENTS
         Local_Main_Node := Old_Main_Node;
         Url_Data :=
           Append_Child
             (N => Url_Node,
              New_Child =>
                Create_Element(Doc => Local_Sitemap, Tag_Name => "loc"));
         Url_Text :=
           Append_Child
             (N => Url_Data,
              New_Child =>
                Create_Text_Node(Doc => Local_Sitemap, Data => Url));
         Url_Data :=
           Append_Child
             (N => Url_Node,
              New_Child =>
                Create_Element(Doc => Local_Sitemap, Tag_Name => "lastmod"));
         Url_Text :=
           Append_Child
             (N => Url_Data,
              New_Child =>
                Create_Text_Node(Doc => Local_Sitemap, Data => Last_Modified));
         --## rule on ASSIGNMENTS
         if Change_Frequency /= "" then
            Url_Data :=
              Append_Child
                (N => Url_Node,
                 New_Child =>
                   Create_Element
                     (Doc => Local_Sitemap, Tag_Name => "changefreq"));
            Url_Text :=
              Append_Child
                (N => Url_Data,
                 New_Child =>
                   Create_Text_Node
                     (Doc => Local_Sitemap, Data => Change_Frequency));
         end if;
         if Page_Priority /= "" then
            Url_Data :=
              Append_Child
                (N => Url_Node,
                 New_Child =>
                   Create_Element
                     (Doc => Local_Sitemap, Tag_Name => "priority"));
            Url_Text :=
              Append_Child
                (N => Url_Data,
                 New_Child =>
                   Create_Text_Node
                     (Doc => Local_Sitemap, Data => Page_Priority));
         end if;
      end if;
      Set_Sitemap(New_Sitemap => Local_Sitemap);
      Set_Main_Node(New_Main_Node => Local_Main_Node);
   end Add_Page_To_Sitemap;

   procedure Save_Sitemap is
      use Ada.Text_IO;
      use Ada.Text_IO.Text_Streams;

      Sitemap_File: File_Type;
   begin
      if not Yass_Conf.Sitemap_Enabled then
         return;
      end if;
      -- If the sitemap file not exists - create or open existing robot.txt file and append address to the sitemap
      if not Exists(Name => To_String(Source => Get_Sitemap_File_Name)) then
         if Exists
             (Name =>
                Containing_Directory
                  (Name => To_String(Source => Get_Sitemap_File_Name)) &
                Dir_Separator & "robots.txt") then
            Open
              (File => Sitemap_File, Mode => Append_File,
               Name =>
                 Containing_Directory
                   (Name => To_String(Source => Get_Sitemap_File_Name)) &
                 Dir_Separator & "robots.txt");
         else
            Create
              (File => Sitemap_File, Mode => Append_File,
               Name =>
                 Containing_Directory
                   (Name => To_String(Source => Get_Sitemap_File_Name)) &
                 Dir_Separator & "robots.txt");
         end if;
         Put_Line
           (File => Sitemap_File,
            Item =>
              "Sitemap: " & To_String(Source => Yass_Conf.Base_Url) &
              "/sitemap.xml");
         Close(File => Sitemap_File);
      end if;
      -- Save the sitemap to the file
      Create
        (File => Sitemap_File, Mode => Out_File,
         Name => To_String(Source => Get_Sitemap_File_Name));
      Write
        (Stream => Stream(File => Sitemap_File), N => Get_Sitemap, Pretty_Print => True);
      Close(File => Sitemap_File);
   end Save_Sitemap;

end Sitemaps;
