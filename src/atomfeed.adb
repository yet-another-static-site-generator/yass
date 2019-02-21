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

with Ada.Calendar.Formatting;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
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
      FeedData: DOM.Core.Element;
      FeedText: Text;
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
         FeedData := Create_Element(Feed, "id");
         FeedData := Append_Child(MainNode, FeedData);
         FeedText := Create_Text_Node(Feed, To_String(YassConfig.BaseURL));
         FeedText := Append_Child(FeedData, FeedText);
         FeedData := Create_Element(Feed, "title");
         FeedData := Append_Child(MainNode, FeedData);
         FeedText := Create_Text_Node(Feed, To_String(YassConfig.SiteName));
         FeedText := Append_Child(FeedData, FeedText);
         FeedData := Create_Element(Feed, "updated");
         FeedData := Append_Child(MainNode, FeedData);
         FeedText := Create_Text_Node(Feed, "0000-00-00T00:00:00Z");
         FeedText := Append_Child(FeedData, FeedText);
      end if;
   end StartAtomFeed;

   procedure AddPageToFeed(FileName: String;
      Titles: in out String_Container.Vector) is
      NodesList, ChildrenList: Node_List;
      Updated: String :=
        Ada.Calendar.Formatting.Image(Modification_Time(FileName)) & "Z";
      Url: constant String :=
        To_String(YassConfig.BaseURL) & "/" &
        Slice
          (To_Unbounded_String(FileName),
           Length
             (SiteDirectory & Dir_Separator & YassConfig.OutputDirectory &
              Dir_Separator) +
           1,
           FileName'Length);
      EntryNode, EntryData: DOM.Core.Element;
      TitleIndex: Positive;
      EntryText: Text;
   begin
      if YassConfig.AtomFeedSource = To_Unbounded_String("none") or
        (YassConfig.AtomFeedSource /= To_Unbounded_String("tags")
         and then Index(FileName, To_String(YassConfig.AtomFeedSource), 1) =
           0) then
         return;
      end if;
      Updated(11) := 'T';
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Feed, "updated");
      Set_Node_Value(First_Child(Item(NodesList, 0)), Updated);
      NodesList := DOM.Core.Documents.Get_Elements_By_Tag_Name(Feed, "title");
      for I in 0 .. Length(NodesList) - 1 loop
         TitleIndex := Titles.First_Index;
         while TitleIndex <= Titles.Last_Index loop
            if Node_Value(First_Child(Item(NodesList, I))) =
              Titles(TitleIndex) then
               EntryNode := Parent_Node(Item(NodesList, I));
               ChildrenList := Child_Nodes(EntryNode);
               for J in 0 .. Length(ChildrenList) - 1 loop
                  if Node_Name(Item(ChildrenList, J)) = "updated" then
                     Set_Node_Value
                       (First_Child(Item(ChildrenList, J)), Updated);
                  elsif Node_Name(Item(ChildrenList, J)) = "id" then
                     Set_Node_Value(First_Child(Item(ChildrenList, J)), Url);
                  end if;
               end loop;
               Titles.Delete(TitleIndex);
               if String_Container.Length(Titles) = 0 then
                  return;
               end if;
               exit;
            end if;
            TitleIndex := TitleIndex + 1;
         end loop;
      end loop;
      for EntryTitle of Titles loop
         EntryNode := Create_Element(Feed, "entry");
         EntryNode := Append_Child(MainNode, EntryNode);
         EntryData := Create_Element(Feed, "id");
         EntryData := Append_Child(EntryNode, EntryData);
         EntryText := Create_Text_Node(Feed, Url);
         EntryText := Append_Child(EntryData, EntryText);
         EntryData := Create_Element(Feed, "title");
         EntryData := Append_Child(EntryNode, EntryData);
         EntryText := Create_Text_Node(Feed, EntryTitle);
         EntryText := Append_Child(EntryData, EntryText);
         EntryData := Create_Element(Feed, "updated");
         EntryData := Append_Child(EntryNode, EntryData);
         EntryText := Create_Text_Node(Feed, Updated);
         EntryText := Append_Child(EntryData, EntryText);
      end loop;
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
