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

package body AtomFeed is

   Feed: Document;
   FeedFileName: Unbounded_String;
   MainNode: DOM.Core.Element;

   procedure StartAtomFeed is
      AtomFile: File_Input;
      Reader: Tree_Reader;
      NewFeed: DOM_Implementation;
      NodesList: Node_List;
   begin
      if YassConfig.AtomFeedSource = To_Unbounded_String("none") then
         return;
      end if;
      FeedFileName :=
        SiteDirectory & Dir_Separator & YassConfig.OutputDirectory &
        To_Unbounded_String(Dir_Separator & "atom.xml");
      if Exists(To_String(FeedFileName)) then
         Open(To_String(FeedFileName), AtomFile);
         Parse(Reader, AtomFile);
         Close(AtomFile);
         Feed := Get_Tree(Reader);
         NodesList :=
           DOM.Core.Documents.Get_Elements_By_Tag_Name(Feed, "feed");
         MainNode := Item(NodesList, 0);
         Set_Attribute(MainNode, "xmlns", "http://www.w3.org/2005/Atom");
      else
         Feed := Create_Document(NewFeed);
         MainNode := Create_Element(Feed, "feed");
         Set_Attribute(MainNode, "xmlns", "http://www.w3.org/2005/Atom");
         MainNode := Append_Child(Feed, MainNode);
      end if;
   end StartAtomFeed;

   procedure AddPageToFeed(FileName, NewFileName: String) is
   begin
      if YassConfig.AtomFeedSource = To_Unbounded_String("none") then
         return;
      end if;
   end AddPageToFeed;

   procedure SaveAtomFeed is
      AtomFile: File_Type;
   begin
      if YassConfig.AtomFeedSource = To_Unbounded_String("none") then
         return;
      end if;
      Create(AtomFile, Out_File, To_String(FeedFileName));
      Write(Stream => Stream(AtomFile), N => Feed, Pretty_Print => True);
      Close(AtomFile);
   end SaveAtomFeed;

end AtomFeed;
