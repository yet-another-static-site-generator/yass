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
with AWS.Templates; use AWS.Templates;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Config; use Config;
with Sitemaps; use Sitemaps;

package body Pages is

   subtype size_t is unsigned_long;
   LayoutNotFound: exception;

   function cmark_markdown_to_html(text: chars_ptr; len: size_t;
      options: int) return chars_ptr with
      Import => True,
      Convention => C,
      External_Name => "cmark_markdown_to_html";

   procedure CreatePage(FileName, Directory: String) is
      Layout, Content: Unbounded_String;
      PageFile: File_Type;
      Tags: Translate_Set;
      OutputDirectory: constant Unbounded_String :=
        SiteDirectory & Dir_Separator & YassConfig.OutputDirectory &
        Delete(To_Unbounded_String(Directory), 1, Length(SiteDirectory));
      NewFileName: constant String :=
        To_String(OutputDirectory) & Dir_Separator &
        Ada.Directories.Base_Name(FileName) & ".html";
      PageTableTags: TableTags_Container.Map;
      procedure AddTag(Name, Value: String) is
      begin
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
            Insert(Tags, Assoc(Name, Integer'Value(Value)));
         end if;
      exception
         when Constraint_Error =>
            Insert(Tags, Assoc(Name, Value));
      end AddTag;
   begin
      declare
         Data: Unbounded_String;
         StartIndex: Natural;
      begin
         Open(PageFile, In_File, FileName);
         while not End_Of_File(PageFile) loop
            Data := To_Unbounded_String(Encode(Get_Line(PageFile)));
            if Length(Data) > 2 then
               if Slice(Data, 1, 3) = "-- " then
                  if Index(Data, "layout:", 1) > 0 then
                     Data := Unbounded_Slice(Data, 12, Length(Data));
                     Layout :=
                       SiteDirectory & Dir_Separator &
                       YassConfig.LayoutsDirectory & Dir_Separator & Data &
                       To_Unbounded_String(".html");
                     if not Ada.Directories.Exists(To_String(Layout)) then
                        Close(PageFile);
                        raise LayoutNotFound
                          with Filename & """. Selected layout file """ &
                          To_String(Layout);
                     end if;
                  else
                     StartIndex := Index(Data, ":", 1);
                     if StartIndex > 0 then
                        AddTag
                          (Slice(Data, 4, StartIndex - 1),
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
      AddPage(NewFileName);
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
   end CreatePage;

   procedure CopyFile(FileName, Directory: String) is
      OutputDirectory: constant Unbounded_String :=
        SiteDirectory & Dir_Separator & YassConfig.OutputDirectory &
        Delete(To_Unbounded_String(Directory), 1, Length(SiteDirectory));
   begin
      Create_Path(To_String(OutputDirectory));
      Copy_File
        (FileName,
         To_String(OutputDirectory) & Dir_Separator & Simple_Name(FileName));
      if Extension(FileName) = ".html" then
         AddPage
           (To_String(OutputDirectory) & Dir_Separator &
            Simple_Name(FileName));
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
                    SiteDirectory & Dir_Separator &
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
