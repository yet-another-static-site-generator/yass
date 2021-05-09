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

   Feed_File_Name: Unbounded_String;
   Entries_List: FeedEntry_Container.Vector;

   function To_Time(Date: String) return Time is
      New_Date: Unbounded_String;
   begin
      if Date'Length > 11 then
         New_Date := To_Unbounded_String(Date(Date'First .. Date'Last - 1));
         Replace_Element(New_Date, 11, ' ');
      else
         New_Date := To_Unbounded_String(Date & " 00:00:00");
      end if;
      return Ada.Calendar.Formatting.Value(To_String(New_Date));
   end To_Time;

   function To_HTTP_Date(Date: Time) return String is
      New_Date: String := Ada.Calendar.Formatting.Image(Date) & "Z";
   begin
      New_Date(11) := 'T';
      return New_Date;
   end To_HTTP_Date;

   procedure Start_Atom_Feed is
      Atom_File: File_Input;
      Reader: Tree_Reader;
      NodesList, ChildrenNodes, AuthorNodes: Node_List;
      Feed: Document;
      TempEntry: Feed_Entry;
      DataNode, AuthorNode: DOM.Core.Element;
      ChildIndex, AuthorNodeIndex: Positive;
   begin
      if YassConfig.AtomFeedSource = To_Unbounded_String("none") then
         SiteTags.Include("AtomLink", "");
         return;
      end if;
      SiteTags.Include
        ("AtomLink",
         "<link rel=""alternate"" type=""application/rss+xml"" title=""" &
         To_String(YassConfig.SiteName) & " Feed"" href=""" &
         To_String(YassConfig.BaseURL) & "/atom.xml"" />");
      Feed_File_Name :=
        YassConfig.OutputDirectory &
        To_Unbounded_String(Dir_Separator & "atom.xml");
      if not Exists(To_String(Feed_File_Name)) then
         return;
      end if;
      Open(To_String(Feed_File_Name), Atom_File);
      Parse(Reader, Atom_File);
      Close(Atom_File);
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
               TempEntry.Entry_Title :=
                 To_Unbounded_String(Node_Value(First_Child(DataNode)));
            elsif Node_Name(DataNode) = "updated" then
               TempEntry.Updated := To_Time(Node_Value(First_Child(DataNode)));
            elsif Node_Name(DataNode) = "author" then
               AuthorNodes := Child_Nodes(DataNode);
               AuthorNodeIndex := 1;
               while AuthorNodeIndex < Length(AuthorNodes) loop
                  AuthorNode := Item(AuthorNodes, AuthorNodeIndex);
                  if Node_Name(AuthorNode) = "name" then
                     TempEntry.Author_Name :=
                       To_Unbounded_String
                         (Node_Value(First_Child(AuthorNode)));
                  elsif Node_Name(AuthorNode) = "email" then
                     TempEntry.Author_Email :=
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
   end Start_Atom_Feed;

   procedure Add_Page_To_Feed
     (File_Name: String; Entries: in out FeedEntry_Container.Vector) is
      Url: constant String :=
        To_String(YassConfig.BaseURL) & "/" &
        Ada.Strings.Unbounded.Slice
          (To_Unbounded_String(File_Name),
           Length(YassConfig.OutputDirectory & Dir_Separator) + 1,
           File_Name'Length);
      DeleteIndex, EntryIndex: Natural := 0;
   begin
      if YassConfig.AtomFeedSource = To_Unbounded_String("none") or
        (YassConfig.AtomFeedSource /= To_Unbounded_String("tags")
         and then Index(File_Name, To_String(YassConfig.AtomFeedSource), 1) =
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
            AtomEntry.Updated := Modification_Time(File_Name);
         end if;
         if AtomEntry.Content = Null_Unbounded_String then
            AtomEntry.Content := AtomEntry.Id;
         end if;
         for I in Entries_List.Iterate loop
            if Entries_List(I).Entry_Title = AtomEntry.Entry_Title then
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
   end Add_Page_To_Feed;

   procedure Save_Atom_Feed is
      Atom_File: File_Type;
      Feed: Document;
      NewFeed: DOM_Implementation;
      MainNode, EntryNode: DOM.Core.Element;
      EntriesAmount: Natural := 0;
   -- Add XML node NodeName with value NodeValue to parent XML node ParentNode
      procedure AddNode
        (NodeName, NodeValue: String; ParentNode: DOM.Core.Element) is
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
      procedure AddLink
        (ParentNode: DOM.Core.Element; URL, Relationship: String) is
         LinkNode: DOM.Core.Element;
      begin
         LinkNode := Create_Element(Feed, "link");
         LinkNode := Append_Child(ParentNode, LinkNode);
         Set_Attribute(LinkNode, "rel", Relationship);
         Set_Attribute(LinkNode, "href", URL);
      end AddLink;
      -- Add author to parent node ParentNode with author name Name and author email Email
      procedure AddAuthor(ParentNode: DOM.Core.Element; Name, Email: String) is
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
         AddNode("title", To_String(FeedEntry.Entry_Title), EntryNode);
         AddNode("updated", To_HTTP_Date(FeedEntry.Updated), EntryNode);
         AddNode("content", To_String(FeedEntry.Content), EntryNode);
         AddLink(EntryNode, To_String(FeedEntry.Id), "alternate");
         if FeedEntry.Author_Name /= Null_Unbounded_String or
           FeedEntry.Author_Email /= Null_Unbounded_String then
            AddAuthor
              (EntryNode, To_String(FeedEntry.Author_Name),
               To_String(FeedEntry.Author_Email));
         end if;
         if FeedEntry.Summary /= Null_Unbounded_String then
            AddNode("summary", To_String(FeedEntry.Summary), EntryNode);
         end if;
         EntriesAmount := EntriesAmount + 1;
         exit when EntriesAmount = YassConfig.AtomFeedAmount;
      end loop;
      Create(Atom_File, Out_File, To_String(Feed_File_Name));
      Write(Stream => Stream(Atom_File), N => Feed, Pretty_Print => True);
      Close(Atom_File);
   end Save_Atom_Feed;

end AtomFeed;
