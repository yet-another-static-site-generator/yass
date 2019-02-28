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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar.Formatting;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with DOM.Core; use DOM.Core;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Readers; use DOM.Readers;
with Input_Sources.File; use Input_Sources.File;
with Config; use Config;

package body AtomFeed is

   FeedFileName: Unbounded_String;
   Entries_List: FeedEntry_Container.Vector;

   function To_Time(Date: String) return Time is
      NewDate: String := Date(Date'First .. Date'Last - 1);
   begin
      NewDate(NewDate'First + 10) := ' ';
      return Ada.Calendar.Formatting.Value(NewDate);
   end To_Time;

   function To_HTTP_Date(Date: Time) return String is
      NewDate: String := Ada.Calendar.Formatting.Image(Date) & "Z";
   begin
      NewDate(11) := 'T';
      return NewDate;
   end To_HTTP_Date;

   procedure StartAtomFeed is
      AtomFile: File_Input;
      Reader: Tree_Reader;
      NodesList, ChildrenNodes: Node_List;
      Feed: Document;
      TempEntry: FeedEntry;
      DataNode: DOM.Core.Element;
   begin
      if YassConfig.AtomFeedSource = To_Unbounded_String("none") then
         return;
      end if;
      FeedFileName :=
        YassConfig.OutputDirectory &
        To_Unbounded_String(Dir_Separator & "atom.xml");
      if Exists(To_String(FeedFileName)) then
         Open(To_String(FeedFileName), AtomFile);
         Parse(Reader, AtomFile);
         Close(AtomFile);
         Feed := Get_Tree(Reader);
         NodesList :=
           DOM.Core.Documents.Get_Elements_By_Tag_Name(Feed, "entry");
         for I in 0 .. Length(NodesList) - 1 loop
            TempEntry := (Null_Unbounded_String, Null_Unbounded_String, Clock);
            ChildrenNodes := Child_Nodes(Item(NodesList, I));
            for J in 1 .. Length(ChildrenNodes) - 1 loop
               if J rem 2 /= 0 then
                  DataNode := Item(ChildrenNodes, J);
                  if Node_Name(DataNode) = "id" then
                     TempEntry.Id :=
                       To_Unbounded_String(Node_Value(First_Child(DataNode)));
                  elsif Node_Name(DataNode) = "title" then
                     TempEntry.EntryTitle :=
                       To_Unbounded_String(Node_Value(First_Child(DataNode)));
                  elsif Node_Name(DataNode) = "updated" then
                     TempEntry.Updated :=
                       To_Time(Node_Value(First_Child(DataNode)));
                  end if;
               end if;
            end loop;
            Entries_List.Append(New_Item => TempEntry);
         end loop;
      end if;
   end StartAtomFeed;

   procedure AddPageToFeed(FileName: String;
      Entries: in out FeedEntry_Container.Vector) is
      Url: constant String :=
        To_String(YassConfig.BaseURL) & "/" &
        Ada.Strings.Unbounded.Slice
          (To_Unbounded_String(FileName),
           Length(YassConfig.OutputDirectory & Dir_Separator) + 1,
           FileName'Length);
      DeleteIndex, EntryIndex: Natural := 0;
   begin
      if YassConfig.AtomFeedSource = To_Unbounded_String("none") or
        (YassConfig.AtomFeedSource /= To_Unbounded_String("tags")
         and then Index(FileName, To_String(YassConfig.AtomFeedSource), 1) =
           0) then
         return;
      end if;
      if FeedEntry_Container.Length(Entries) > 1
        and then Entries(1).Updated < Entries(2).Updated then
         Entries.Reverse_Elements;
      end if;
      for AtomEntry of Entries loop
         if AtomEntry.Id = Null_Unbounded_String then
            AtomEntry.Id := To_Unbounded_String(Url);
         elsif Index(AtomEntry.Id, Url, 1) = 0 then
            AtomEntry.Id := To_Unbounded_String(Url) & AtomEntry.Id;
         end if;
         if AtomEntry.Updated = Time_Of(1901, 1, 1) then
            AtomEntry.Updated := Modification_Time(FileName);
         end if;
         for I in Entries_List.Iterate loop
            if Entries_List(I).EntryTitle = AtomEntry.EntryTitle then
               DeleteIndex := FeedEntry_Container.To_Index(I);
               exit;
            end if;
         end loop;
         if DeleteIndex > 0 then
            Entries_List.Delete(DeleteIndex);
            DeleteIndex := 0;
         end if;
         EntryIndex := Entries_List.First_Index;
         while EntryIndex <= Entries_List.Last_Index loop
            if Entries_List(EntryIndex).Updated < AtomEntry.Updated then
               Entries_List.Insert(EntryIndex, AtomEntry);
               exit;
            end if;
            EntryIndex := EntryIndex + 1;
         end loop;
         if EntryIndex > Entries_List.Last_Index then
            Entries_List.Append(New_Item => AtomEntry);
         end if;
      end loop;
   end AddPageToFeed;

   procedure SaveAtomFeed is
      AtomFile: File_Type;
      Feed: Document;
      NewFeed: DOM_Implementation;
      MainNode, EntryNode: DOM.Core.Element;
      EntriesAmount: Natural := 0;
      procedure AddNode(NodeName, NodeValue: String;
         ParentNode: Dom.Core.Element) is
         FeedData: DOM.Core.Element;
         FeedText: Text;
      begin
         FeedData := Create_Element(Feed, NodeName);
         FeedData := Append_Child(ParentNode, FeedData);
         FeedText := Create_Text_Node(Feed, NodeValue);
         if Append_Child(FeedData, FeedText) /= null then
            return;
         end if;
      end AddNode;
   begin
      if YassConfig.AtomFeedSource = To_Unbounded_String("none") or
        FeedEntry_Container.Length(Entries_List) = 0 then
         return;
      end if;
      Feed := Create_Document(NewFeed);
      MainNode := Create_Element(Feed, "feed");
      Set_Attribute(MainNode, "xmlns", "http://www.w3.org/2005/Atom");
      MainNode := Append_Child(Feed, MainNode);
      AddNode("id", To_String(YassConfig.BaseURL) & "/", MainNode);
      AddNode("title", To_String(YassConfig.SiteName), MainNode);
      AddNode("updated", To_HTTP_Date(Entries_List(1).Updated), MainNode);
      for FeedEntry of Entries_List loop
         EntryNode := Create_Element(Feed, "entry");
         EntryNode := Append_Child(MainNode, EntryNode);
         AddNode("id", To_String(FeedEntry.Id), EntryNode);
         AddNode("title", To_String(FeedEntry.EntryTitle), EntryNode);
         AddNode("updated", To_HTTP_Date(FeedEntry.Updated), EntryNode);
         EntriesAmount := EntriesAmount + 1;
         exit when EntriesAmount > YassConfig.AtomFeedAmount;
      end loop;
      Create(AtomFile, Out_File, To_String(FeedFileName));
      Write(Stream => Stream(AtomFile), N => Feed, Pretty_Print => True);
      Close(AtomFile);
   end SaveAtomFeed;

end AtomFeed;
