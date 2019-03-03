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
         if Name = "title" and Value /= "[]" then
            AtomEntries.Prepend
              (New_Item =>
                 (EntryTitle => To_Unbounded_String(Value),
                  Id => Null_Unbounded_String,
                  Updated => Time_Of(1901, 1, 1)));
         end if;
         if Name = "id" and Value /= "[]" then
            AtomEntries(AtomEntries.First_Index).Id :=
              To_Unbounded_String(Value);
         end if;
         if Name = "updated" and Value /= "[]" then
            AtomEntries(AtomEntries.First_Index).Updated := To_Time(Value);
         end if;
         if To_Lower(Value) = "true" then
            Insert(Tags, Assoc(Name, True));
         elsif To_Lower(Value) = "false" then
            Insert(Tags, Assoc(Name, False));
         elsif Value = "[]" then
            TableTags_Container.Include(PageTableTags, Name, +"");
            Clear(PageTableTags(Name));
         elsif TableTags_Container.Contains(PageTableTags, Name) then
            PageTableTags(Name) := PageTableTags(Name) & Value;
         else
            if Is_Number(Value) then
               Insert(Tags, Assoc(Name, Integer'Value(Value)));
            else
               Insert(Tags, Assoc(Name, Value));
            end if;
         end if;
      exception
         when Constraint_Error =>
            raise InvalidValue with """" & Name & """ value """ & Value & """";
      end AddTag;
   begin
      declare
         Data: Unbounded_String;
         StartIndex: Natural;
         StartPos: constant Positive := Length(YassConfig.MarkdownComment);
      begin
         Open(PageFile, In_File, FileName);
         while not End_Of_File(PageFile) loop
            Data := To_Unbounded_String(Encode(Get_Line(PageFile)));
            if Length(Data) > 2 then
               if Unbounded_Slice(Data, 1, StartPos) =
                 YassConfig.MarkdownComment then
                  if Index(Data, "layout:", 1) = (StartPos + 2) then
                     Data :=
                       Unbounded_Slice(Data, (StartPos + 10), Length(Data));
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
               else
                  Append(Content, Data);
                  Append(Content, LF);
               end if;
            else
               Append(Content, Data);
               Append(Content, LF);
            end if;
         end loop;
         Close(PageFile);
      end;
      Insert
        (Tags,
         Assoc
           ("Content",
            Value
              (cmark_markdown_to_html
                 (New_String(To_String(Content)), size_t(Length(Content)),
                  0))));
      for I in SiteTags.Iterate loop
         AddTag(Tags_Container.Key(I), SiteTags(I));
      end loop;
      for I in PageTableTags.Iterate loop
         Insert(Tags, Assoc(TableTags_Container.Key(I), PageTableTags(I)));
      end loop;
      for I in GlobalTableTags.Iterate loop
         Insert(Tags, Assoc(TableTags_Container.Key(I), PageTableTags(I)));
      end loop;
      Create_Path(To_String(OutputDirectory));
      Create(PageFile, Append_File, NewFileName);
      Put(PageFile, Decode(Parse(To_String(Layout), Tags)));
      Close(PageFile);
      if InSitemap then
         AddPageToSitemap
           (NewFileName, To_String(ChangeFrequency), To_String(PagePriority));
      end if;
      AddPageToFeed(NewFileName, AtomEntries);
      Set("YASSFILE", NewFileName);
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
   end CopyFile;

   procedure CreateEmptyIndexFile(DirectoryName: String) is
      IndexFile: File_Type;
   begin
      Create
        (IndexFile, Append_File, DirectoryName & Dir_Separator & "index.md");
      Put_Line
        (IndexFile,
         "-- All lines which starts with double minus sign are comments and ignored by program. Unless they have colon sign. Then they are tags definition.");
      Put_Line
        (IndexFile,
         "-- Ada Web Server template which will be used as HTML template for this file. Required for each file");
      Put_Line(IndexFile, "-- layout: default");
      Put_Line
        (IndexFile,
         "-- You may add as many tags as you want and they can be in any place in file, not only at beginning. Tags can be 4 types: strings, boolean, numeric or composite.");
      Put_Line
        (IndexFile,
         "-- First 3 types of tags are in Name: Value scheme. For strings it can be any alphanumeric value without new line sign. For boolean it must be ""true"" or ""false"", for numeric any number. Program will detect self which type of tag is and properly set it. It always fall back to string value.");
      Put_Line
        (IndexFile,
         "-- Composite tags first must be initialized with Name: [] then just add as many as you want values to it by Name: Value scheme.");
      Put_Line
        (IndexFile,
         "-- For more informations about tags please check program documentation.");
      Put_Line
        (IndexFile,
         "-- If you have enabled creation of sitemap in the project config file, you can set some sitemap parameters too. They are defined in this same way like tags, with ParameterName: Value.");
      Put_Line
        (IndexFile,
         "-- priority - The priority of this URL relative to other URLs on your site, value between 0.0 and 1.0.");
      Put_Line
        (IndexFile,
         "-- changefreq - How frequently the page is likely to change, value can be always, hourly, daily, weekly, monthly, yearly or never.");
      Put_Line
        (IndexFile,
         "-- For more informations how this options works, please look at the program documentation.");
      Put_Line
        (IndexFile,
         "-- Additionally, you can exclude this file from adding to sitemap by setting option insitemap: false.");
      Put_Line
        (IndexFile,
         "-- If you have enabled creating Atom feed for the site, you must specify ""title"" tag for this page. If you will use this file as a main source of Atom feed, then you must add ""title"" tag for each section which will be used as source for Atom feed entry.");
      Put_Line(IndexFile, "-- title: New page");
      Put_Line
        (IndexFile,
         "-- You can without problem delete all this comments from this file.");
      Close(IndexFile);
   end CreateEmptyIndexFile;

   function GetLayoutName(FileName: String) return String is
      PageFile: File_Type;
      Data, Layout: Unbounded_String;
   begin
      Open(PageFile, In_File, FileName);
      while not End_Of_File(PageFile) loop
         Data := To_Unbounded_String(Encode(Get_Line(PageFile)));
         if Length(Data) > 2 then
            if Slice(Data, 1, 3) = "-- " then
               if Index(Data, "layout:", 1) > 0 then
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
            end if;
         end if;
      end loop;
      Close(PageFile);
      return "";
   end GetLayoutName;

end Pages;
