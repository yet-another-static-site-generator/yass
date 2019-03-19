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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.UTF_Encoding.Strings; use Ada.Strings.UTF_Encoding.Strings;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Calendar; use Ada.Calendar;
with AWS.Templates; use AWS.Templates;
with AWS.Templates.Utils; use AWS.Templates.Utils;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Config; use Config;
with Sitemaps; use Sitemaps;
with AtomFeed; use AtomFeed;
with Modules; use Modules;

package body Pages is

   subtype size_t is unsigned_long;
   LayoutNotFound: exception;

   function cmark_markdown_to_html(text: chars_ptr; len: size_t;
      options: int) return chars_ptr with
      Import => True,
      Convention => C,
      External_Name => "cmark_markdown_to_html";

   procedure CreatePage(FileName, Directory: String) is
      Layout, Content, ChangeFrequency, PagePriority: Unbounded_String;
      PageFile: File_Type;
      Tags: Translate_Set;
      OutputDirectory: constant Unbounded_String :=
        YassConfig.OutputDirectory &
        Delete(To_Unbounded_String(Directory), 1, Length(SiteDirectory));
      NewFileName: constant String :=
        To_String(OutputDirectory) & Dir_Separator &
        Ada.Directories.Base_Name(FileName) & ".html";
      PageTags: Tags_Container.Map;
      PageTableTags: TableTags_Container.Map;
      FrequencyValues: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("always"), To_Unbounded_String("hourly"),
         To_Unbounded_String("daily"), To_Unbounded_String("weekly"),
         To_Unbounded_String("monthly"), To_Unbounded_String("yearly"),
         To_Unbounded_String("never"));
      ValidValue: Boolean := False;
      InSitemap: Boolean := True;
      AtomEntries: FeedEntry_Container.Vector;
      SitemapInvalidValue, InvalidValue: exception;
      procedure AddTag(Name, Value: String) is
      begin
         if Value /= "[]" then
            if Name = "title" then
               AtomEntries.Prepend
                 (New_Item =>
                    (EntryTitle => To_Unbounded_String(Value),
                     Id => Null_Unbounded_String,
                     Updated => Time_Of(1901, 1, 1),
                     AuthorName => Null_Unbounded_String,
                     AuthorEmail => Null_Unbounded_String,
                     Summary => Null_Unbounded_String,
                     Content => Null_Unbounded_String));
            elsif Name = "id" then
               AtomEntries(AtomEntries.First_Index).Id :=
                 To_Unbounded_String(Value);
            elsif Name = "updated" then
               AtomEntries(AtomEntries.First_Index).Updated := To_Time(Value);
            elsif Name = "author" then
               AtomEntries(AtomEntries.First_Index).AuthorName :=
                 To_Unbounded_String(Value);
            elsif Name = "authoremail" then
               AtomEntries(AtomEntries.First_Index).AuthorEmail :=
                 To_Unbounded_String(Value);
            elsif Name = "summary" then
               AtomEntries(AtomEntries.First_Index).Summary :=
                 To_Unbounded_String(Value);
            elsif Name = "content" then
               AtomEntries(AtomEntries.First_Index).Content :=
                 To_Unbounded_String(Value);
            end if;
            if TableTags_Container.Contains(PageTableTags, Name) then
               PageTableTags(Name) := PageTableTags(Name) & Value;
            else
               Tags_Container.Include(PageTags, Name, Value);
            end if;
         else
            TableTags_Container.Include(PageTableTags, Name, +"");
            Clear(PageTableTags(Name));
         end if;
      exception
         when Constraint_Error =>
            raise InvalidValue with """" & Name & """ value """ & Value & """";
      end AddTag;
      procedure InsertTags(TagsList: Tags_Container.Map) is
      begin
         for I in TagsList.Iterate loop
            if To_Lower(TagsList(I)) = "true" then
               Insert(Tags, Assoc(Tags_Container.Key(I), True));
            elsif To_Lower(TagsList(I)) = "false" then
               Insert(Tags, Assoc(Tags_Container.Key(I), False));
            else
               if Is_Number(TagsList(I)) then
                  Insert
                    (Tags,
                     Assoc(Tags_Container.Key(I), Integer'Value(TagsList(I))));
               else
                  Insert(Tags, Assoc(Tags_Container.Key(I), TagsList(I)));
               end if;
            end if;
         end loop;
      end InsertTags;
   begin
      declare
         Data: Unbounded_String;
         StartIndex: Natural;
         StartPos: constant Positive := Length(YassConfig.MarkdownComment);
      begin
         Open(PageFile, In_File, FileName);
         while not End_Of_File(PageFile) loop
            Data := To_Unbounded_String(Encode(Get_Line(PageFile)));
            if Length(Data) < 3 then
               Append(Content, Data);
               Append(Content, LF);
               goto End_Of_Loop;
            end if;
            if Unbounded_Slice(Data, 1, StartPos) /=
              YassConfig.MarkdownComment then
               Append(Content, Data);
               Append(Content, LF);
               goto End_Of_Loop;
            end if;
            if Index(Data, "layout:", 1) = (StartPos + 2) then
               Data := Unbounded_Slice(Data, (StartPos + 10), Length(Data));
               Layout :=
                 YassConfig.LayoutsDirectory & Dir_Separator & Data &
                 To_Unbounded_String(".html");
               if not Ada.Directories.Exists(To_String(Layout)) then
                  Close(PageFile);
                  raise LayoutNotFound
                    with Filename & """. Selected layout file """ &
                    To_String(Layout);
               end if;
            elsif Index(Data, "changefreq:", 1) = (StartPos + 2) then
               ChangeFrequency :=
                 Unbounded_Slice(Data, (StartPos + 14), Length(Data));
               for I in FrequencyValues'Range loop
                  if ChangeFrequency = FrequencyValues(I) then
                     ValidValue := True;
                     exit;
                  end if;
               end loop;
               if not ValidValue then
                  raise SitemapInvalidValue
                    with "Invalid value for changefreq";
               end if;
               ValidValue := False;
            elsif Index(Data, "priority:", 1) = (StartPos + 2) then
               PagePriority :=
                 Unbounded_Slice(Data, (StartPos + 11), Length(Data));
               if Float'Value(To_String(PagePriority)) < 0.0 or
                 Float'Value(To_String(PagePriority)) > 1.0 then
                  raise SitemapInvalidValue
                    with "Invalid value for page priority";
               end if;
            elsif Index(Data, "insitemap:", 1) = (StartPos + 2) then
               if To_Lower(Slice(Data, (StartPos + 13), Length(Data))) =
                 "false" then
                  InSitemap := False;
               end if;
            else
               StartIndex := Index(Data, ":", (StartPos + 2));
               if StartIndex > Index(Data, " ", (StartPos + 2)) then
                  StartIndex := 0;
               end if;
               if StartIndex > 0 then
                  AddTag
                    (Slice(Data, (StartPos + 2), StartIndex - 1),
                     Slice(Data, StartIndex + 2, Length(Data)));
               end if;
            end if;
            <<End_Of_Loop>>
         end loop;
         Close(PageFile);
      end;
      Tags_Container.Include
        (PageTags, "Content",
         Value
           (cmark_markdown_to_html
              (New_String(To_String(Content)), size_t(Length(Content)), 0)));
      LoadModules("pre", PageTags, PageTableTags);
      Insert(Tags, Assoc("Content", PageTags("Content")));
      InsertTags(SiteTags);
      for I in PageTableTags.Iterate loop
         Insert(Tags, Assoc(TableTags_Container.Key(I), PageTableTags(I)));
      end loop;
      for I in GlobalTableTags.Iterate loop
         Insert(Tags, Assoc(TableTags_Container.Key(I), GlobalTableTags(I)));
      end loop;
      InsertTags(PageTags);
      Create_Path(To_String(OutputDirectory));
      Create(PageFile, Append_File, NewFileName);
      Put(PageFile, Decode(Parse(To_String(Layout), Tags)));
      Close(PageFile);
      if InSitemap then
         AddPageToSitemap
           (NewFileName, To_String(ChangeFrequency), To_String(PagePriority));
      end if;
      if YassConfig.AtomFeedSource = To_Unbounded_String("tags") then
         AtomEntries(AtomEntries.First_Index).Content := Content;
      end if;
      AddPageToFeed(NewFileName, AtomEntries);
      Set("YASSFILE", NewFileName);
      LoadModules("post", PageTags, PageTableTags);
   exception
      when An_Exception : LayoutNotFound =>
         Put_Line
           ("Can't parse """ & Exception_Message(An_Exception) &
            """ does not exists.");
         raise GenerateSiteException;
      when An_Exception : Template_Error =>
         Put_Line(Exception_Message(An_Exception));
         if Ada.Directories.Exists(NewFileName) then
            Close(PageFile);
            Delete_File(NewFileName);
         end if;
         raise GenerateSiteException;
      when An_Exception : SitemapInvalidValue =>
         Put_Line
           ("Can't parse """ & Filename & """. " &
            Exception_Message(An_Exception));
         raise GenerateSiteException;
      when Constraint_Error =>
         Put_Line
           ("Can't parse """ & Filename &
            """. Invalid value for page priority.");
         raise GenerateSiteException;
      when An_Exception : InvalidValue =>
         Put_Line
           ("Can't parse """ & Filename & """. Invalid value for tag: " &
            Exception_Message(An_Exception));
         raise GenerateSiteException;
   end CreatePage;

   procedure CopyFile(FileName, Directory: String) is
      OutputDirectory: constant Unbounded_String :=
        YassConfig.OutputDirectory &
        Delete(To_Unbounded_String(Directory), 1, Length(SiteDirectory));
   begin
      LoadModules("pre", SiteTags, GlobalTableTags);
      Create_Path(To_String(OutputDirectory));
      Copy_File
        (FileName,
         To_String(OutputDirectory) & Dir_Separator & Simple_Name(FileName));
      if Extension(FileName) = "html" then
         AddPageToSitemap
           (To_String(OutputDirectory) & Dir_Separator & Simple_Name(FileName),
            "", "");
      end if;
      Set
        ("YASSFILE",
         To_String(OutputDirectory) & Dir_Separator & Simple_Name(FileName));
      LoadModules("post", SiteTags, GlobalTableTags);
   end CopyFile;

   procedure CreateEmptyFile(FileName: String) is
      IndexFile: File_Type;
      CommentMark: constant String := To_String(YassConfig.MarkdownComment);
   begin
      if Extension(FileName) /= "md" then
         Create(IndexFile, Append_File, FileName & Dir_Separator & "index.md");
      else
         Create(IndexFile, Append_File, FileName);
      end if;
      Put_Line
        (IndexFile,
         CommentMark &
         " All lines which starts with double minus sign are comments and ignored by program. Unless they have colon sign. Then they are tags definition.");
      Put_Line
        (IndexFile,
         CommentMark &
         " Ada Web Server template which will be used as HTML template for this file. Required for each file");
      Put_Line(IndexFile, CommentMark & " layout: default");
      Put_Line
        (IndexFile,
         CommentMark &
         " You may add as many tags as you want and they can be in any place in file, not only at beginning. Tags can be 4 types: strings, boolean, numeric or composite.");
      Put_Line
        (IndexFile,
         CommentMark &
         " First 3 types of tags are in Name: Value scheme. For strings it can be any alphanumeric value without new line sign. For boolean it must be ""true"" or ""false"", for numeric any number. Program will detect self which type of tag is and properly set it. It always fall back to string value.");
      Put_Line
        (IndexFile,
         CommentMark &
         " Composite tags first must be initialized with Name: [] then just add as many as you want values to it by Name: Value scheme.");
      Put_Line
        (IndexFile,
         CommentMark &
         " For more informations about tags please check program documentation.");
      Put_Line
        (IndexFile,
         CommentMark &
         " If you have enabled creation of sitemap in the project config file, you can set some sitemap parameters too. They are defined in this same way like tags, with ParameterName: Value.");
      Put_Line
        (IndexFile,
         CommentMark &
         " priority - The priority of this URL relative to other URLs on your site, value between 0.0 and 1.0.");
      Put_Line
        (IndexFile,
         CommentMark &
         " changefreq - How frequently the page is likely to change, value can be always, hourly, daily, weekly, monthly, yearly or never.");
      Put_Line
        (IndexFile,
         CommentMark &
         " For more informations how this options works, please look at the program documentation.");
      Put_Line
        (IndexFile,
         CommentMark &
         " Additionally, you can exclude this file from adding to sitemap by setting option insitemap: false.");
      Put_Line
        (IndexFile,
         CommentMark &
         " If you have enabled creating Atom feed for the site, you must specify ""title"" tag for this page. If you will use this file as a main source of Atom feed, then you must add ""title"" tag for each section which will be used as source for Atom feed entry. If you want to set author name for Atom feed, you must add ""author"" tag. If you want to set author email for Atom feed, you must add ""authoremail"" tag. If you want to add short entry summary, you must add tag ""summary"". Did that tags will be for whole page or for each entry depends on your Atom feed configuration.");
      Put_Line(IndexFile, CommentMark & " title: New page");
      Put_Line
        (IndexFile,
         CommentMark &
         " You can without problem delete all this comments from this file.");
      Close(IndexFile);
   end CreateEmptyFile;

   function GetLayoutName(FileName: String) return String is
      PageFile: File_Type;
      Data, Layout: Unbounded_String;
      StartPos: constant Positive := Length(YassConfig.MarkdownComment);
   begin
      Open(PageFile, In_File, FileName);
      while not End_Of_File(PageFile) loop
         Data := To_Unbounded_String(Encode(Get_Line(PageFile)));
         if Length(Data) > 2
           and then Unbounded_Slice(Data, 1, StartPos) =
             YassConfig.MarkdownComment
           and then Index(Data, "layout:", 1) = (StartPos + 2) then
            Data := Unbounded_Slice(Data, 12, Length(Data));
            Layout :=
              YassConfig.LayoutsDirectory & Dir_Separator & Data &
              To_Unbounded_String(".html");
            if not Ada.Directories.Exists(To_String(Layout)) then
               Close(PageFile);
               raise LayoutNotFound
                 with Filename & """. Selected layout file """ &
                 To_String(Layout);
            end if;
            Close(PageFile);
            return To_String(Layout);
         end if;
      end loop;
      Close(PageFile);
      return "";
   end GetLayoutName;

end Pages;
