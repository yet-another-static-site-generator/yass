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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Readers; use DOM.Readers;
with Input_Sources.File; use Input_Sources.File;
with Config; use Config;

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
      if not YassConfig.SitemapEnabled then
         return;
      end if;
      FileName :=
        SiteDirectory & Dir_Separator & YassConfig.OutputDirectory &
        To_Unbounded_String(Dir_Separator & "sitemap.xml");
      if Exists(To_String(FileName)) then
         Open(To_String(FileName), SitemapFile);
         Parse(Reader, SitemapFile);
         Close(SitemapFile);
         Sitemap := Get_Tree(Reader);
         NodesList :=
           DOM.Core.Documents.Get_Elements_By_Tag_Name(Sitemap, "urlset");
         MainNode := Item(NodesList, 0);
      else
         Sitemap := Create_Document(NewSitemap);
         MainNode := Create_Element(Sitemap, "urlset");
         Set_Attribute
           (MainNode, "xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9");
         MainNode := Append_Child(Sitemap, MainNode);
      end if;
   end StartSitemap;

   procedure AddPageToSitemap(FileName: String) is
      Url: constant String :=
        To_String(YassConfig.BaseURL) & "/" &
        Slice
          (To_Unbounded_String(FileName),
           Length
             (SiteDirectory & Dir_Separator & YassConfig.OutputDirectory &
              Dir_Separator) +
           1,
           FileName'Length);
      URLsList: Node_List;
      Added: Boolean := False;
      URLNode, URLData, OldMainNode: DOM.Core.Element;
      URLText: Text;
   begin
      if not YassConfig.SitemapEnabled then
         return;
      end if;
      UrlsList := DOM.Core.Documents.Get_Elements_By_Tag_Name(Sitemap, "loc");
      for I in 0 .. Length(URLsList) - 1 loop
         if Node_Value(First_Child(Item(URLsList, I))) = Url then
            Added := True;
            exit;
         end if;
      end loop;
      if not Added then
         URLNode := Create_Element(Sitemap, "url");
         OldMainNode := MainNode;
         MainNode := Append_Child(MainNode, URLNode);
         MainNode := OldMainNode;
         URLData := Create_Element(Sitemap, "loc");
         URLData := Append_Child(URLNode, URLData);
         URLText := Create_Text_Node(Sitemap, Url);
         URLText := Append_Child(URLData, URLText);
      end if;
   end AddPageToSitemap;

   procedure SaveSitemap is
      SitemapFile: File_Type;
   begin
      if not YassConfig.SitemapEnabled then
         return;
      end if;
      Create(SitemapFile, Out_File, To_String(FileName));
      Write(Stream => Stream(SitemapFile), N => Sitemap, Pretty_Print => True);
      Close(SitemapFile);
   end SaveSitemap;

end Sitemaps;
