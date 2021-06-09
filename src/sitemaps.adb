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
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Readers; use DOM.Readers;
with Input_Sources.File; use Input_Sources.File;
with Config; use Config;
with AtomFeed; use AtomFeed;

package body Sitemaps is

   Sitemap: Document;
   FileName: Unbounded_String;
   MainNode: DOM.Core.Element;

   procedure StartSitemap is
      SitemapFile: File_Input;
      Reader: Tree_Reader;
      NewSitemap: DOM_Implementation;
      NodesList: Node_List;
   begin
      if not Yass_Config.Sitemap_Enabled then
         return;
      end if;
      FileName :=
        Yass_Config.Output_Directory &
        To_Unbounded_String(Dir_Separator & "sitemap.xml");
      -- Load existing sitemap data
      if Exists(To_String(FileName)) then
         Open(To_String(FileName), SitemapFile);
         Parse(Reader, SitemapFile);
         Close(SitemapFile);
         Sitemap := Get_Tree(Reader);
         NodesList :=
           DOM.Core.Documents.Get_Elements_By_Tag_Name(Sitemap, "urlset");
         MainNode := Item(NodesList, 0);
         Set_Attribute
           (MainNode, "xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9");
         -- Create new sitemap data
      else
         Sitemap := Create_Document(NewSitemap);
         MainNode := Create_Element(Sitemap, "urlset");
         Set_Attribute
           (MainNode, "xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9");
         MainNode := Append_Child(Sitemap, MainNode);
      end if;
   end StartSitemap;

   procedure AddPageToSitemap
     (FileName, ChangeFrequency, PagePriority: String) is
      Url: constant String :=
        To_String(Yass_Config.Base_Url) & "/" &
        Slice
          (To_Unbounded_String(FileName),
           Length(Yass_Config.Output_Directory & Dir_Separator) + 1,
           FileName'Length);
      URLsList, ChildrenList: Node_List;
      Added, FrequencyUpdated, PriorityUpdated: Boolean := False;
      URLNode, URLData, OldMainNode, RemoveFrequency,
      RemovePriority: DOM.Core.Element;
      URLText: Text;
      LastModified: constant String := To_HTTP_Date(Clock);
   begin
      if not Yass_Config.Sitemap_Enabled then
         return;
      end if;
      URLsList := DOM.Core.Documents.Get_Elements_By_Tag_Name(Sitemap, "loc");
      for I in 0 .. Length(URLsList) - 1 loop
         if Node_Value(First_Child(Item(URLsList, I))) /= Url then
            goto End_Of_Loop;
         end if;
         -- Update sitemap entry if exists
         URLNode := Parent_Node(Item(URLsList, I));
         ChildrenList := Child_Nodes(URLNode);
         for J in 0 .. Length(ChildrenList) - 1 loop
            if Node_Name(Item(ChildrenList, J)) = "lastmod" then
               URLText := First_Child(Item(ChildrenList, J));
               Set_Node_Value(URLText, LastModified);
            elsif Node_Name(Item(ChildrenList, J)) = "changefreq" then
               if ChangeFrequency /= "" then
                  URLText := First_Child(Item(ChildrenList, J));
                  Set_Node_Value(URLText, ChangeFrequency);
               else
                  RemoveFrequency := Item(ChildrenList, J);
               end if;
               FrequencyUpdated := True;
            elsif Node_Name(Item(ChildrenList, J)) = "priority" then
               if PagePriority /= "" then
                  URLText := First_Child(Item(ChildrenList, J));
                  Set_Node_Value(URLText, PagePriority);
               else
                  RemovePriority := Item(ChildrenList, J);
               end if;
               PriorityUpdated := True;
            end if;
         end loop;
         if ChangeFrequency /= "" and not FrequencyUpdated then
            URLData := Create_Element(Sitemap, "changefreq");
            URLData := Append_Child(URLNode, URLData);
            URLText := Create_Text_Node(Sitemap, ChangeFrequency);
            URLText := Append_Child(URLData, URLText);
         end if;
         if PagePriority /= "" and not PriorityUpdated then
            URLData := Create_Element(Sitemap, "priority");
            URLData := Append_Child(URLNode, URLData);
            URLText := Create_Text_Node(Sitemap, PagePriority);
            URLText := Append_Child(URLData, URLText);
         end if;
         if RemoveFrequency /= null then
            URLNode := Remove_Child(URLNode, RemoveFrequency);
         end if;
         if RemovePriority /= null then
            URLNode := Remove_Child(URLNode, RemovePriority);
         end if;
         Added := True;
         exit;
         <<End_Of_Loop>>
      end loop;
      -- Add new sitemap entry
      if not Added then
         URLNode := Create_Element(Sitemap, "url");
         OldMainNode := MainNode;
         MainNode := Append_Child(MainNode, URLNode);
         MainNode := OldMainNode;
         URLData := Create_Element(Sitemap, "loc");
         URLData := Append_Child(URLNode, URLData);
         URLText := Create_Text_Node(Sitemap, Url);
         URLText := Append_Child(URLData, URLText);
         URLData := Create_Element(Sitemap, "lastmod");
         URLData := Append_Child(URLNode, URLData);
         URLText := Create_Text_Node(Sitemap, LastModified);
         URLText := Append_Child(URLData, URLText);
         if ChangeFrequency /= "" then
            URLData := Create_Element(Sitemap, "changefreq");
            URLData := Append_Child(URLNode, URLData);
            URLText := Create_Text_Node(Sitemap, ChangeFrequency);
            URLText := Append_Child(URLData, URLText);
         end if;
         if PagePriority /= "" then
            URLData := Create_Element(Sitemap, "priority");
            URLData := Append_Child(URLNode, URLData);
            URLText := Create_Text_Node(Sitemap, PagePriority);
            URLText := Append_Child(URLData, URLText);
         end if;
      end if;
   end AddPageToSitemap;

   procedure SaveSitemap is
      SitemapFile: File_Type;
   begin
      if not Yass_Config.Sitemap_Enabled then
         return;
      end if;
      -- If the sitemap file not exists - create or open existing robot.txt file and append address to the sitemap
      if not Exists(To_String(FileName)) then
         if Exists
             (Containing_Directory(To_String(FileName)) & Dir_Separator &
              "robots.txt") then
            Open
              (SitemapFile, Append_File,
               Containing_Directory(To_String(FileName)) & Dir_Separator &
               "robots.txt");
         else
            Create
              (SitemapFile, Append_File,
               Containing_Directory(To_String(FileName)) & Dir_Separator &
               "robots.txt");
         end if;
         Put_Line
           (SitemapFile,
            "Sitemap: " & To_String(Yass_Config.Base_Url) & "/sitemap.xml");
         Close(SitemapFile);
      end if;
      -- Save the sitemap to the file
      Create(SitemapFile, Out_File, To_String(FileName));
      Write(Stream => Stream(SitemapFile), N => Sitemap, Pretty_Print => True);
      Close(SitemapFile);
   end SaveSitemap;

end Sitemaps;
