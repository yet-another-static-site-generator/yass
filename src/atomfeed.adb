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

   -- ****iv* AtomFeed/Feed_File_Name
   -- FUNCTION
   -- The name of the Atom feed file of the website
   -- SOURCE
   Feed_File_Name: Unbounded_String;
   -- ****

   -- ****iv* AtomFeed/Entries_List
   -- FUNCTION
   -- The list of Atom entries for the website
   -- SOURCE
   Entries_List: FeedEntry_Container.Vector;
   -- ****

   -- ****if* AtomFeed/Get_Entries_List
   -- FUNCTION
   -- Get the current list of Atom entries for the website
   -- RESULT
   -- List of Atom entries for the website
   -- SOURCE
   function Get_Entries_List return FeedEntry_Container.Vector is
      -- ****
   begin
      return Entries_List;
   end Get_Entries_List;

   -- ****if* AtomFeed/Set_Entries_List
   -- FUNCTION
   -- Set the new values for the list of current Atom entries for the website
   -- PARAMETERS
   -- New_List - The Atom entries list which will be used as the main list
   -- SOURCE
   procedure Set_Entries_List(New_List: FeedEntry_Container.Vector) is
      -- ****
   begin
      Entries_List := New_List;
   end Set_Entries_List;

   function To_Time(Date: String) return Time is
      New_Date: Unbounded_String;
   begin
      if Date'Length > 11 then
         New_Date :=
           To_Unbounded_String(Source => Date(Date'First .. Date'Last - 1));
         Replace_Element(Source => New_Date, Index => 11, By => ' ');
      else
         New_Date := To_Unbounded_String(Source => Date & " 00:00:00");
      end if;
      return
        Ada.Calendar.Formatting.Value(Date => To_String(Source => New_Date));
   end To_Time;

   function To_HTTP_Date(Date: Time) return String is
      New_Date: String := Ada.Calendar.Formatting.Image(Date => Date) & "Z";
   begin
      New_Date(11) := 'T';
      return New_Date;
   end To_HTTP_Date;

   procedure Start_Atom_Feed is
      Atom_File: File_Input;
      --## rule off IMPROPER_INITIALIZATION
      Reader: Tree_Reader;
      Nodes_List, Children_Nodes, Author_Nodes: Node_List;
      --## rule on IMPROPER_INITIALIZATION
      Feed: Document;
      Temp_Entry: Feed_Entry := Empty_Feed_Entry;
      Data_Node, Author_Node: DOM.Core.Element;
      Child_Index, Author_Node_Index: Positive := 1;
      Local_Entries: FeedEntry_Container.Vector := Get_Entries_List;
   begin
      if YassConfig.AtomFeedSource = To_Unbounded_String(Source => "none") then
         SiteTags.Include(Key => "AtomLink", New_Item => "");
         return;
      end if;
      SiteTags.Include
        (Key => "AtomLink",
         New_Item =>
           "<link rel=""alternate"" type=""application/rss+xml"" title=""" &
           To_String(Source => YassConfig.SiteName) & " Feed"" href=""" &
           To_String(Source => YassConfig.BaseURL) & "/atom.xml"" />");
      Feed_File_Name :=
        YassConfig.OutputDirectory &
        To_Unbounded_String(Source => Dir_Separator & "atom.xml");
      if not Exists(Name => To_String(Source => Feed_File_Name)) then
         return;
      end if;
      Open
        (Filename => To_String(Source => Feed_File_Name), Input => Atom_File);
      --## rule off IMPROPER_INITIALIZATION
      Parse(Parser => Reader, Input => Atom_File);
      --## rule on IMPROPER_INITIALIZATION
      Close(Input => Atom_File);
      Feed :=
        Get_Tree(Read => Reader); --## rule line off IMPROPER_INITIALIZATION
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Feed, Tag_Name => "entry");
      Load_Atom_Entries_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Temp_Entry :=
           (Id => Null_Unbounded_String, Entry_Title => Null_Unbounded_String,
            Updated => Clock, Author_Name => Null_Unbounded_String,
            Author_Email => Null_Unbounded_String,
            Summary => Null_Unbounded_String,
            Content => Null_Unbounded_String);
         Children_Nodes :=
           Child_Nodes(N => Item(List => Nodes_List, Index => I));
         Child_Index := 1;
         Set_Atom_Entry_Loop :
         while Child_Index < Length(List => Children_Nodes) loop
            Data_Node := Item(List => Children_Nodes, Index => Child_Index);
            if Node_Name(N => Data_Node) = "id" then
               Temp_Entry.Id :=
                 To_Unbounded_String
                   (Source => Node_Value(N => First_Child(N => Data_Node)));
            elsif Node_Name(N => Data_Node) = "title" then
               Temp_Entry.Entry_Title :=
                 To_Unbounded_String
                   (Source => Node_Value(N => First_Child(N => Data_Node)));
            elsif Node_Name(N => Data_Node) = "updated" then
               Temp_Entry.Updated :=
                 To_Time(Date => Node_Value(N => First_Child(N => Data_Node)));
            elsif Node_Name(N => Data_Node) = "author" then
               Author_Nodes := Child_Nodes(N => Data_Node);
               Author_Node_Index := 1;
               Set_Author_Node_Loop :
               while Author_Node_Index < Length(List => Author_Nodes) loop
                  Author_Node :=
                    Item(List => Author_Nodes, Index => Author_Node_Index);
                  if Node_Name(N => Author_Node) = "name" then
                     Temp_Entry.Author_Name :=
                       To_Unbounded_String
                         (Source =>
                            Node_Value(N => First_Child(N => Author_Node)));
                  elsif Node_Name(N => Author_Node) = "email" then
                     Temp_Entry.Author_Email :=
                       To_Unbounded_String
                         (Source =>
                            Node_Value(N => First_Child(N => Author_Node)));
                  end if;
                  Author_Node_Index := Author_Node_Index + 2;
               end loop Set_Author_Node_Loop;
            elsif Node_Name(N => Data_Node) = "summary" then
               Temp_Entry.Summary :=
                 To_Unbounded_String
                   (Source => Node_Value(N => First_Child(N => Data_Node)));
            elsif Node_Name(N => Data_Node) = "content" then
               Temp_Entry.Content :=
                 To_Unbounded_String
                   (Source => Node_Value(N => First_Child(N => Data_Node)));
            end if;
            Child_Index := Child_Index + 2;
         end loop Set_Atom_Entry_Loop;
         Local_Entries.Append(New_Item => Temp_Entry);
      end loop Load_Atom_Entries_Loop;
      Set_Entries_List(New_List => Local_Entries);
   end Start_Atom_Feed;

   procedure Add_Page_To_Feed
     (File_Name: String; Entries: in out FeedEntry_Container.Vector) is
      Url: constant String :=
        To_String(Source => YassConfig.BaseURL) & "/" &
        Ada.Strings.Unbounded.Slice
          (Source => To_Unbounded_String(Source => File_Name),
           Low =>
             Length(Source => YassConfig.OutputDirectory & Dir_Separator) + 1,
           High => File_Name'Length);
      Delete_Index, Entry_Index: Natural := 0;
      Local_Entries: FeedEntry_Container.Vector := Get_Entries_List;
   begin
      if YassConfig.AtomFeedSource = To_Unbounded_String(Source => "none") or
        (YassConfig.AtomFeedSource /= To_Unbounded_String(Source => "tags")
         and then
           Index
             (Source => File_Name,
              Pattern => To_String(Source => YassConfig.AtomFeedSource),
              From => 1) =
           0) then
         return;
      end if;
      if FeedEntry_Container.Length(Container => Entries) > 1
        and then Entries(1).Updated < Entries(2).Updated then
         Entries.Reverse_Elements;
      end if;
      Add_Page_To_Feed_Loop :
      for AtomEntry of Entries loop
         if AtomEntry.Id = Null_Unbounded_String then
            AtomEntry.Id := To_Unbounded_String(Source => Url);
         elsif Index(Source => AtomEntry.Id, Pattern => Url, From => 1) =
           0 then
            AtomEntry.Id :=
              To_Unbounded_String(Source => Url) & "#" & AtomEntry.Id;
         end if;
         if AtomEntry.Updated =
           Time_Of(Year => 1_901, Month => 1, Day => 1) then
            AtomEntry.Updated := Modification_Time(Name => File_Name);
         end if;
         if AtomEntry.Content = Null_Unbounded_String then
            AtomEntry.Content := AtomEntry.Id;
         end if;
         Find_Delete_Index_Loop :
         for I in Local_Entries.Iterate loop
            if Local_Entries(I).Entry_Title = AtomEntry.Entry_Title then
               Delete_Index := FeedEntry_Container.To_Index(Position => I);
               exit Find_Delete_Index_Loop;
            end if;
         end loop Find_Delete_Index_Loop;
         if Delete_Index > 0 then
            Local_Entries.Delete(Index => Delete_Index);
            Delete_Index := 0;
         end if;
         Entry_Index := Get_Entries_List.First_Index;
         Move_Atom_Entries_Loop :
         for I in Local_Entries.Iterate loop
            if Local_Entries(I).Updated < AtomEntry.Updated then
               Entry_Index := FeedEntry_Container.To_Index(Position => I);
               Local_Entries.Insert(Before => I, New_Item => AtomEntry);
               exit Move_Atom_Entries_Loop;
            end if;
         end loop Move_Atom_Entries_Loop;
         if Entry_Index > Local_Entries.Last_Index then
            Local_Entries.Append(New_Item => AtomEntry);
         end if;
      end loop Add_Page_To_Feed_Loop;
      Set_Entries_List(New_List => Local_Entries);
   end Add_Page_To_Feed;

   procedure Save_Atom_Feed is
      Atom_File: File_Type;
      Feed: Document;
      New_Feed: DOM_Implementation; --## rule line off IMPROPER_INITIALIZATION
      Main_Node, Entry_Node: DOM.Core.Element;
      Entries_Amount: Natural := 0;
   -- Add XML node Node_Name with value Node_Value to parent XML node Parent_Node
      procedure Add_Node
        (Node_Name, Node_Value: String; Parent_Node: DOM.Core.Element) is
         Feed_Text: Text;
         Feed_Data: DOM.Core.Element;
      begin
         Feed_Data :=
           Append_Child
             (N => Parent_Node,
              New_Child => Create_Element(Doc => Feed, Tag_Name => Node_Name));
         Feed_Text := Create_Text_Node(Doc => Feed, Data => Node_Value);
         if Append_Child(N => Feed_Data, New_Child => Feed_Text) /= null then
            return;
         end if;
      end Add_Node;
      -- Add link entry to parent node Parent_Node with url URL and relationship Relationship
      procedure Add_Link
        (Parent_Node: DOM.Core.Element; Url, Relationship: String) is
         Link_Node: DOM.Core.Element;
      begin
         Link_Node := Append_Child(Parent_Node, Create_Element(Feed, "link"));
         Set_Attribute(Link_Node, "rel", Relationship);
         Set_Attribute(Link_Node, "href", Url);
      end Add_Link;
      -- Add author to parent node Parent_Node with author name Name and author email Email
      procedure AddAuthor
        (Parent_Node: DOM.Core.Element; Name, Email: String) is
         Author_Node: DOM.Core.Element;
      begin
         Author_Node := Create_Element(Feed, "author");
         Author_Node := Append_Child(Parent_Node, Author_Node);
         if Name'Length > 0 then
            Add_Node("name", Name, Author_Node);
         end if;
         if Email'Length > 0 then
            Add_Node("email", Email, Author_Node);
         end if;
      end AddAuthor;
   begin
      if YassConfig.AtomFeedSource = To_Unbounded_String("none") or
        FeedEntry_Container.Length(Get_Entries_List) = 0 then
         return;
      end if;
      Feed :=
        Create_Document(New_Feed); --## rule line off IMPROPER_INITIALIZATION
      Main_Node := Create_Element(Feed, "feed");
      Set_Attribute(Main_Node, "xmlns", "http://www.w3.org/2005/Atom");
      Main_Node := Append_Child(Feed, Main_Node);
      Add_Link(Main_Node, To_String(YassConfig.BaseURL) & "/atom.xml", "self");
      Add_Node("id", To_String(YassConfig.BaseURL) & "/", Main_Node);
      Add_Node("title", To_String(YassConfig.SiteName), Main_Node);
      Add_Node("updated", To_HTTP_Date(Entries_List(1).Updated), Main_Node);
      AddAuthor
        (Main_Node, To_String(YassConfig.AuthorName),
         To_String(YassConfig.AuthorEmail));
      for FeedEntry of Get_Entries_List loop
         Entry_Node := Create_Element(Feed, "entry");
         Entry_Node := Append_Child(Main_Node, Entry_Node);
         Add_Node("id", To_String(FeedEntry.Id), Entry_Node);
         Add_Node("title", To_String(FeedEntry.Entry_Title), Entry_Node);
         Add_Node("updated", To_HTTP_Date(FeedEntry.Updated), Entry_Node);
         Add_Node("content", To_String(FeedEntry.Content), Entry_Node);
         Add_Link(Entry_Node, To_String(FeedEntry.Id), "alternate");
         if FeedEntry.Author_Name /= Null_Unbounded_String or
           FeedEntry.Author_Email /= Null_Unbounded_String then
            AddAuthor
              (Entry_Node, To_String(FeedEntry.Author_Name),
               To_String(FeedEntry.Author_Email));
         end if;
         if FeedEntry.Summary /= Null_Unbounded_String then
            Add_Node("summary", To_String(FeedEntry.Summary), Entry_Node);
         end if;
         Entries_Amount := Entries_Amount + 1;
         exit when Entries_Amount = YassConfig.AtomFeedAmount;
      end loop;
      Create(Atom_File, Out_File, To_String(Feed_File_Name));
      Write(Stream => Stream(Atom_File), N => Feed, Pretty_Print => True);
      Close(Atom_File);
   end Save_Atom_Feed;

end AtomFeed;
