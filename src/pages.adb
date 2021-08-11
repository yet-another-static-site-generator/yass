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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.UTF_Encoding.Strings; use Ada.Strings.UTF_Encoding.Strings;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Calendar; use Ada.Calendar;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with AWS.Templates; use AWS.Templates;
with AWS.Templates.Utils; use AWS.Templates.Utils;
with Config; use Config;
with Sitemaps; use Sitemaps;
with AtomFeed; use AtomFeed;
with Modules; use Modules;

package body Pages is

   subtype Size_T is unsigned_long;
   Layout_Not_Found: exception;

   function Cmark_Markdown_To_Html
     (Text: chars_ptr; Len: Size_T; Options: int) return chars_ptr with
      Import => True,
      Convention => C,
      External_Name => "cmark_markdown_to_html";

   procedure Create_Page(File_Name, Directory: String) is
      Layout, Content, Change_Frequency, Page_Priority: Unbounded_String :=
        Null_Unbounded_String;
      Page_File: File_Type;
      Tags: Translate_Set := Null_Set;
      Output_Directory: constant Unbounded_String :=
        Yass_Config.Output_Directory &
        Delete
          (Source => To_Unbounded_String(Source => Directory), From => 1,
           Through => Length(Source => Site_Directory));
      New_File_Name: constant String :=
        To_String(Source => Output_Directory) & Dir_Separator &
        Ada.Directories.Base_Name(Name => File_Name) & ".html";
      Page_Tags: Tags_Container.Map := Tags_Container.Empty_Map;
      Page_Table_Tags: TableTags_Container.Map :=
        TableTags_Container.Empty_Map;
      Frequency_Values: constant array(1 .. 7) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "always"),
         2 => To_Unbounded_String(Source => "hourly"),
         3 => To_Unbounded_String(Source => "daily"),
         4 => To_Unbounded_String(Source => "weekly"),
         5 => To_Unbounded_String(Source => "monthly"),
         6 => To_Unbounded_String(Source => "yearly"),
         7 => To_Unbounded_String(Source => "never"));
      In_Sitemap: Boolean := True;
      Atom_Entries: FeedEntry_Container.Vector :=
        FeedEntry_Container.Empty_Vector;
      Sitemap_Invalid_Value, Invalid_Value: exception;
      -- Add tag to the page template tags lists (simple or composite).
      -- Name: name of the tag
      -- Value: value of the tag
      procedure Add_Tag(Name, Value: String) is
      begin
         -- Create new composite template tag
         if Value = "[]" then
            Page_Table_Tags.Include(Key => Name, New_Item => +"");
            Clear(T => Page_Table_Tags(Name));
            return;
         end if;
         -- Add values to Atom feed entries for the page
         if Name = "title" then
            Atom_Entries.Prepend
              (New_Item =>
                 (Entry_Title => To_Unbounded_String(Source => Value),
                  Id => Null_Unbounded_String,
                  Updated => Time_Of(Year => 1_901, Month => 1, Day => 1),
                  Author_Name => Null_Unbounded_String,
                  Author_Email => Null_Unbounded_String,
                  Summary => Null_Unbounded_String,
                  Content => Null_Unbounded_String));
         elsif Name = "id" then
            Atom_Entries(Atom_Entries.First_Index).Id :=
              To_Unbounded_String(Source => Value);
         elsif Name = "updated" then
            Atom_Entries(Atom_Entries.First_Index).Updated :=
              To_Time(Date => Value);
         elsif Name = "author" then
            Atom_Entries(Atom_Entries.First_Index).Author_Name :=
              To_Unbounded_String(Source => Value);
         elsif Name = "authoremail" then
            Atom_Entries(Atom_Entries.First_Index).Author_Email :=
              To_Unbounded_String(Source => Value);
         elsif Name = "summary" then
            Atom_Entries(Atom_Entries.First_Index).Summary :=
              To_Unbounded_String(Source => Value);
         elsif Name = "content" then
            Atom_Entries(Atom_Entries.First_Index).Content :=
              To_Unbounded_String(Source => Value);
         end if;
         -- Add value for composite tag
         if Page_Table_Tags.Contains(Key => Name) then
            Page_Table_Tags(Name) := Page_Table_Tags(Name) & Value;
            -- Add value for simple tag
         else
            Page_Tags.Include(Key => Name, New_Item => Value);
         end if;
      exception
         when Constraint_Error =>
            raise Invalid_Value
              with """" & Name & """ value """ & Value & """";
      end Add_Tag;
      -- Insert selected list of tags TagsList to templates
      procedure Insert_Tags(TagsList: Tags_Container.Map) is
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
      end Insert_Tags;
   begin
      declare
         Data: Unbounded_String;
         StartIndex: Natural;
         StartPos: constant Positive := Length(Yass_Config.Markdown_Comment);
         ValidValue: Boolean := False;
      begin
         -- Read selected markdown file
         Open(Page_File, In_File, File_Name);
         while not End_Of_File(Page_File) loop
            Data := To_Unbounded_String(Encode(Get_Line(Page_File)));
            if Length(Data) < 3 then
               Append(Content, Data);
               Append(Content, LF);
               goto End_Of_Loop;
            end if;
            if Unbounded_Slice(Data, 1, StartPos) /=
              Yass_Config.Markdown_Comment then
               Append(Content, Data);
               Append(Content, LF);
               goto End_Of_Loop;
            end if;
            -- Get the page template layout
            if Index(Data, "layout:", 1) = (StartPos + 2) then
               Data := Unbounded_Slice(Data, (StartPos + 10), Length(Data));
               Layout :=
                 Yass_Config.Layouts_Directory & Dir_Separator & Data &
                 To_Unbounded_String(".html");
               if not Ada.Directories.Exists(To_String(Layout)) then
                  Close(Page_File);
                  raise Layout_Not_Found
                    with File_Name & """. Selected layout file """ &
                    To_String(Layout);
               end if;
               -- Set update frequency for the page in the sitemap
            elsif Index(Data, "changefreq:", 1) = (StartPos + 2) then
               Change_Frequency :=
                 Unbounded_Slice(Data, (StartPos + 14), Length(Data));
               for I in Frequency_Values'Range loop
                  if Change_Frequency = Frequency_Values(I) then
                     ValidValue := True;
                     exit;
                  end if;
               end loop;
               if not ValidValue then
                  raise Sitemap_Invalid_Value
                    with "Invalid value for changefreq";
               end if;
               ValidValue := False;
               -- Set priority for the page in the sitemap
            elsif Index(Data, "priority:", 1) = (StartPos + 2) then
               Page_Priority :=
                 Unbounded_Slice(Data, (StartPos + 11), Length(Data));
               begin
                  if Float'Value(To_String(Page_Priority)) < 0.0 or
                    Float'Value(To_String(Page_Priority)) > 1.0 then
                     raise Sitemap_Invalid_Value
                       with "Invalid value for page priority";
                  end if;
               exception
                  when Constraint_Error =>
                     raise Sitemap_Invalid_Value
                       with "Invalid value for page priority";
               end;
               -- Check if the page is excluded from the sitemap
            elsif Index(Data, "insitemap:", 1) = (StartPos + 2) then
               if To_Lower(Slice(Data, (StartPos + 13), Length(Data))) =
                 "false" then
                  In_Sitemap := False;
               end if;
               -- Add tag to the page tags lists
            else
               StartIndex := Index(Data, ":", (StartPos + 2));
               if StartIndex > Index(Data, " ", (StartPos + 2)) then
                  StartIndex := 0;
               end if;
               if StartIndex > 0 then
                  Add_Tag
                    (Slice(Data, (StartPos + 2), StartIndex - 1),
                     Slice(Data, StartIndex + 2, Length(Data)));
               end if;
            end if;
            <<End_Of_Loop>>
         end loop;
         Close(Page_File);
      end;
      -- Convert markdown to HTML
      Tags_Container.Include
        (Page_Tags, "Content",
         Value
           (Cmark_Markdown_To_Html
              (New_String(To_String(Content)), Size_T(Length(Content)), 0)));
      -- Load the program modules with 'pre' hook
      Load_Modules("pre", Page_Tags, Page_Table_Tags);
      -- Insert tags to template
      Insert(Tags, Assoc("Content", Page_Tags("Content")));
      Insert_Tags(Site_Tags);
      if not Exists(Tags, "canonicallink") then
         Insert
           (Tags,
            Assoc
              ("canonicallink",
               To_String(Yass_Config.Base_Url) & "/" &
               Slice
                 (To_Unbounded_String(New_File_Name),
                  Length(Yass_Config.Output_Directory & Dir_Separator) + 1,
                  New_File_Name'Length)));
      end if;
      if not Exists(Tags, "author") then
         Insert(Tags, Assoc("author", To_String(Yass_Config.Author_Name)));
      end if;
      if not Exists(Tags, "description")
        and then Site_Tags.Contains("Description") then
         Insert(Tags, Assoc("description", Site_Tags("Description")));
      end if;
      for I in Page_Table_Tags.Iterate loop
         Insert(Tags, Assoc(TableTags_Container.Key(I), Page_Table_Tags(I)));
      end loop;
      for I in Global_Table_Tags.Iterate loop
         Insert(Tags, Assoc(TableTags_Container.Key(I), Global_Table_Tags(I)));
      end loop;
      Insert_Tags(Page_Tags);
      -- Create HTML file in Output_Directory
      Create_Path(To_String(Output_Directory));
      Create(Page_File, Append_File, New_File_Name);
      Put(Page_File, Decode(Parse(To_String(Layout), Tags)));
      Close(Page_File);
      -- Add the page to the sitemap
      if In_Sitemap then
         AddPageToSitemap
           (New_File_Name, To_String(Change_Frequency),
            To_String(Page_Priority));
      end if;
      -- Add the page to the Atom feed
      if Yass_Config.Atom_Feed_Source = To_Unbounded_String("tags") then
         Atom_Entries(Atom_Entries.First_Index).Content := Content;
      end if;
      Add_Page_To_Feed(New_File_Name, Atom_Entries);
      Set("YASSFILE", New_File_Name);
      -- Load the program modules with 'post' hook
      Load_Modules("post", Page_Tags, Page_Table_Tags);
   exception
      when An_Exception : Layout_Not_Found =>
         Put_Line
           ("Can't parse """ & Exception_Message(An_Exception) &
            """ does not exists.");
         raise Generate_Site_Exception;
      when An_Exception : Template_Error =>
         Put_Line(Exception_Message(An_Exception));
         if Ada.Directories.Exists(New_File_Name) then
            Close(Page_File);
            Delete_File(New_File_Name);
         end if;
         raise Generate_Site_Exception;
      when An_Exception : Sitemap_Invalid_Value =>
         Put_Line
           ("Can't parse """ & File_Name & """. " &
            Exception_Message(An_Exception));
         raise Generate_Site_Exception;
      when An_Exception : Invalid_Value =>
         Put_Line
           ("Can't parse """ & File_Name & """. Invalid value for tag: " &
            Exception_Message(An_Exception));
         raise Generate_Site_Exception;
   end Create_Page;

   procedure Copy_File(File_Name, Directory: String) is
      Output_Directory: constant Unbounded_String :=
        Yass_Config.Output_Directory &
        Delete(To_Unbounded_String(Directory), 1, Length(Site_Directory));
      Page_Tags: Tags_Container.Map := Tags_Container.Empty_Map;
      Page_Table_Tags: TableTags_Container.Map :=
        TableTags_Container.Empty_Map;
   begin
      -- Load the program modules with 'pre' hook
      Load_Modules("pre", Page_Tags, Page_Table_Tags);
      -- Copy the file to output directory
      Create_Path(To_String(Output_Directory));
      Ada.Directories.Copy_File
        (File_Name,
         To_String(Output_Directory) & Dir_Separator & Simple_Name(File_Name));
      if Extension(File_Name) = "html" then
         AddPageToSitemap
           (To_String(Output_Directory) & Dir_Separator &
            Simple_Name(File_Name),
            "", "");
      end if;
      Set
        ("YASSFILE",
         To_String(Output_Directory) & Dir_Separator & Simple_Name(File_Name));
      -- Load the program modules with 'post' hook
      Load_Modules("post", Page_Tags, Page_Table_Tags);
   end Copy_File;

   procedure Create_Empty_File(File_Name: String) is
      IndexFile: File_Type;
      CommentMark: constant String := To_String(Yass_Config.Markdown_Comment);
   begin
      if Extension(File_Name) /= "md" then
         Create
           (IndexFile, Append_File, File_Name & Dir_Separator & "index.md");
      else
         Create(IndexFile, Append_File, File_Name);
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
         " You may add as many tags as you want, and they can be in any place in file, not only at beginning. Tags can be 4 types: strings, boolean, numeric or composite.");
      Put_Line
        (IndexFile,
         CommentMark &
         " First 3 types of tags are in Name: Value scheme. For strings, it can be any alphanumeric value without new line sign. For boolean it must be ""true"" or ""false"", for numeric any number. Program will detect self which type of tag is and properly set it. It always falls back to string value.");
      Put_Line
        (IndexFile,
         CommentMark &
         " Composite tags first must be initialized with Name: [] then just add as many as you want values to it by Name: Value scheme.");
      Put_Line
        (IndexFile,
         CommentMark &
         " For more information about tags please check program documentation.");
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
         " For more information how this options works, please look at the program documentation.");
      Put_Line
        (IndexFile,
         CommentMark &
         " Additionally, you can exclude this file from adding to sitemap by setting option insitemap: false.");
      Put_Line
        (IndexFile,
         CommentMark &
         " If you have enabled creating Atom feed for the site, you must specify ""title"" tag for this page. If you want to use this file as a main source of Atom feed, then you must add ""title"" tag for each section which will be used as source for Atom feed entry. If you want to set author name for Atom feed, you must add ""author"" tag or setting Author from configuration file will be used. When you want to set author email for Atom feed, you must add ""authoremail"" tag. If you want to add short entry summary, you must add tag ""summary"". Do that tag will be for whole page or for each entry depends on your Atom feed configuration.");
      Put_Line
        (IndexFile,
         CommentMark &
         " You can also specify canonical link for the page. If you don't set it here, the program will generate it automatically. To set the default canonical link for the page set tag ""canonicallink"". It must be a full URL (with https://).");
      Put_Line
        (IndexFile,
         CommentMark &
         " By setting ""author"" tag for the page, you can overwrite the configuration setting for meta tag author for the page.");
      Put_Line(IndexFile, CommentMark & " title: New page");
      Put_Line
        (IndexFile,
         CommentMark &
         " You can without problem delete all this comments from this file.");
      Close(IndexFile);
   end Create_Empty_File;

   function Get_Layout_Name(File_Name: String) return String is
      Page_File: File_Type;
      Data, Layout: Unbounded_String;
      StartPos: constant Positive := Length(Yass_Config.Markdown_Comment);
   begin
      Open(Page_File, In_File, File_Name);
      while not End_Of_File(Page_File) loop
         Data := To_Unbounded_String(Encode(Get_Line(Page_File)));
         if Length(Data) > 2
           and then Unbounded_Slice(Data, 1, StartPos) =
             Yass_Config.Markdown_Comment
           and then Index(Data, "layout:", 1) = (StartPos + 2) then
            Data := Unbounded_Slice(Data, 12, Length(Data));
            Layout :=
              Yass_Config.Layouts_Directory & Dir_Separator & Data &
              To_Unbounded_String(".html");
            if not Ada.Directories.Exists(To_String(Layout)) then
               Close(Page_File);
               raise Layout_Not_Found
                 with File_Name & """. Selected layout file """ &
                 To_String(Layout);
            end if;
            Close(Page_File);
            return To_String(Layout);
         end if;
      end loop;
      Close(Page_File);
      return "";
   end Get_Layout_Name;

end Pages;
