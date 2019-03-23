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
      NodesList, ChildrenNodes, AuthorNodes: Node_List;
      Feed: Document;
      TempEntry: FeedEntry;
      DataNode, AuthorNode: DOM.Core.Element;
      ChildIndex, AuthorNodeIndex: Positive;
   begin
      if YassConfig.AtomFeedSource = To_Unbounded_String("none") then
         return;
      end if;
      FeedFileName :=
        YassConfig.OutputDirectory &
        To_Unbounded_String(Dir_Separator & "atom.xml");
      if not Exists(To_String(FeedFileName)) then
         return;
      end if;
      Open(To_String(FeedFileName), AtomFile);
      Parse(Reader, AtomFile);
      Close(AtomFile);
      Feed := Get_Tree(Reader);
      NodesList := DOM.Core.Documents.Get_Elements_By_Tag_Name(Feed, "entry");
      for I in 0 .. Length(NodesList) - 1 loop
         TempEntry :=
           (Null_Unbounded_String, Null_Unbounded_String, Clock,
            Null_Unbounded_String, Null_Unbounded_String,
            Null_Unbounded_String, Null_Unbounded_String);
         ChildrenNodes := Child_Nodes(Item(NodesList, I));
         ChildIndex := 1;
         while ChildIndex < Length(ChildrenNodes) loop
            DataNode := Item(ChildrenNodes, ChildIndex);
            if Node_Name(DataNode) = "id" then
               TempEntry.Id :=
                 To_Unbounded_String(Node_Value(First_Child(DataNode)));
            elsif Node_Name(DataNode) = "title" then
               TempEntry.EntryTitle :=
                 To_Unbounded_String(Node_Value(First_Child(DataNode)));
            elsif Node_Name(DataNode) = "updated" then
               TempEntry.Updated := To_Time(Node_Value(First_Child(DataNode)));
            elsif Node_Name(DataNode) = "author" then
               AuthorNodes := Child_Nodes(DataNode);
               AuthorNodeIndex := 1;
               while AuthorNodeIndex < Length(AuthorNodes) loop
                  AuthorNode := Item(AuthorNodes, AuthorNodeIndex);
                  if Node_Name(AuthorNode) = "name" then
                     TempEntry.AuthorName :=
                       To_Unbounded_String
                         (Node_Value(First_Child(AuthorNode)));
                  elsif Node_Name(AuthorNode) = "email" then
                     TempEntry.AuthorEmail :=
                       To_Unbounded_String
                         (Node_Value(First_Child(AuthorNode)));
                  end if;
                  AuthorNodeIndex := AuthorNodeIndex + 2;
               end loop;
            elsif Node_Name(DataNode) = "summary" then
               TempEntry.Summary :=
                 To_Unbounded_String(Node_Value(First_Child(DataNode)));
            elsif Node_Name(DataNode) = "content" then
               TempEntry.Content :=
                 To_Unbounded_String(Node_Value(First_Child(DataNode)));
            end if;
            ChildIndex := ChildIndex + 2;
         end loop;
         Entries_List.Append(New_Item => TempEntry);
      end loop;
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
            AtomEntry.Id := To_Unbounded_String(Url) & "#" & AtomEntry.Id;
         end if;
         if AtomEntry.Updated = Time_Of(1901, 1, 1) then
            AtomEntry.Updated := Modification_Time(FileName);
         end if;
         if AtomEntry.Content = Null_Unbounded_String then
            AtomEntry.Content := AtomEntry.Id;
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
   -- Add XML node NodeName with value NodeValue to parent XML node ParentNode
      procedure AddNode(NodeName, NodeValue: String;
         ParentNode: Dom.Core.Element) is
         FeedText: Text;
         FeedData: DOM.Core.Element;
      begin
         FeedData := Create_Element(Feed, NodeName);
         FeedData := Append_Child(ParentNode, FeedData);
         FeedText := Create_Text_Node(Feed, NodeValue);
         if Append_Child(FeedData, FeedText) /= null then
            return;
         end if;
      end AddNode;
      -- Add link entry to parent node ParentNode with url URL and relationship Relationship
      procedure AddLink(ParentNode: Dom.Core.Element;
         URL, Relationship: String) is
         LinkNode: DOM.Core.Element;
      begin
         LinkNode := Create_Element(Feed, "link");
         LinkNode := Append_Child(ParentNode, LinkNode);
         Set_Attribute(LinkNode, "rel", Relationship);
         Set_Attribute(LinkNode, "href", URL);
      end AddLink;
      -- Add author to parent node ParentNode with author name Name and author email Email
      procedure AddAuthor(ParentNode: Dom.Core.Element; Name, Email: String) is
         AuthorNode: DOM.Core.Element;
      begin
         AuthorNode := Create_Element(Feed, "author");
         AuthorNode := Append_Child(ParentNode, AuthorNode);
         if Name'Length > 0 then
            AddNode("name", Name, AuthorNode);
         end if;
         if Email'Length > 0 then
            AddNode("email", Email, AuthorNode);
         end if;
      end AddAuthor;
   begin
      if YassConfig.AtomFeedSource = To_Unbounded_String("none") or
        FeedEntry_Container.Length(Entries_List) = 0 then
         return;
      end if;
      Feed := Create_Document(NewFeed);
      MainNode := Create_Element(Feed, "feed");
      Set_Attribute(MainNode, "xmlns", "http://www.w3.org/2005/Atom");
      MainNode := Append_Child(Feed, MainNode);
      AddLink(MainNode, To_String(YassConfig.BaseURL) & "/atom.xml", "self");
      AddNode("id", To_String(YassConfig.BaseURL) & "/", MainNode);
      AddNode("title", To_String(YassConfig.SiteName), MainNode);
      AddNode("updated", To_HTTP_Date(Entries_List(1).Updated), MainNode);
      AddAuthor
        (MainNode, To_String(YassConfig.AuthorName),
         To_String(YassConfig.AuthorEmail));
      for FeedEntry of Entries_List loop
         EntryNode := Create_Element(Feed, "entry");
         EntryNode := Append_Child(MainNode, EntryNode);
         AddNode("id", To_String(FeedEntry.Id), EntryNode);
         AddNode("title", To_String(FeedEntry.EntryTitle), EntryNode);
         AddNode("updated", To_HTTP_Date(FeedEntry.Updated), EntryNode);
         AddNode("content", To_String(FeedEntry.Content), EntryNode);
         AddLink(EntryNode, To_String(FeedEntry.Id), "alternate");
         if FeedEntry.AuthorName /= Null_Unbounded_String or
           FeedEntry.AuthorEmail /= Null_Unbounded_String then
            AddAuthor
              (EntryNode, To_String(FeedEntry.AuthorName),
               To_String(FeedEntry.AuthorEmail));
         end if;
         if FeedEntry.Summary /= Null_Unbounded_String then
            AddNode("summary", To_String(FeedEntry.Summary), EntryNode);
         end if;
         EntriesAmount := EntriesAmount + 1;
         exit when EntriesAmount = YassConfig.AtomFeedAmount;
      end loop;
      Create(AtomFile, Out_File, To_String(FeedFileName));
      Write(Stream => Stream(AtomFile), N => Feed, Pretty_Print => True);
      Close(AtomFile);
   end SaveAtomFeed;

end AtomFeed;
