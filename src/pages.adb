--    Copyright 2019-2021 Bartek thindil Jasicki & 2022-2024 A.J. Ianozi
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

with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Text_IO;

with GNAT.Directory_Operations;

with AWS.Templates.Utils;

with AtomFeed;
with CMark;
with Config;   use Config;
with Modules;  use Modules;
with Sitemaps; use Sitemaps;

package body Pages is

   Layout_Not_Found : exception;

   Dir_Separator : Character renames Gnat.Directory_Operations.Dir_Separator;

   -----------------
   -- Create_Page --
   -----------------

   procedure Create_Page (File_Name : String;
                          Directory : String)
   is
      use Ada.Strings.UTF_Encoding.Strings;
      use Ada.Characters.Handling;
      use Ada.Exceptions;
      use Ada.Directories;
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;

      use AWS.Templates;

      use AtomFeed;

      Layout, Content, Change_Frequency, Page_Priority: Unbounded_String :=
        Null_Unbounded_String;
      Page_File: File_Type;
      Tags: Translate_Set := Null_Set; --## rule line off GLOBAL_REFERENCES
      Output_Directory: constant Unbounded_String :=
        Yass_Conf.Output_Directory &
        Delete
          (Source => To_Unbounded_String(Source => Directory), From => 1,
           Through => Length(Source => Site_Directory));
      New_File_Name: constant String :=
        To_String(Source => Output_Directory) & Dir_Separator &
        Ada.Directories.Base_Name(Name => File_Name) & ".html";
      --## rule off GLOBAL_REFERENCES
      Page_Tags: Tags_Container.Map := Tags_Container.Empty_Map;
      Page_Table_Tags: TableTags_Container.Map :=
        TableTags_Container.Empty_Map;
      --## rule on GLOBAL_REFERENCES
      Frequency_Values: constant array(1 .. 7) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "always"),
         2 => To_Unbounded_String(Source => "hourly"),
         3 => To_Unbounded_String(Source => "daily"),
         4 => To_Unbounded_String(Source => "weekly"),
         5 => To_Unbounded_String(Source => "monthly"),
         6 => To_Unbounded_String(Source => "yearly"),
         7 => To_Unbounded_String(Source => "never"));
      In_Sitemap: Boolean := True;
      --## rule off GLOBAL_REFERENCES
      Atom_Entries: FeedEntry_Container.Vector :=
        FeedEntry_Container.Empty_Vector;
      --## rule on GLOBAL_REFERENCES
      Sitemap_Invalid_Value, Invalid_Value: exception;

      -----------------
      -- Insert_Tags --
      -----------------

      -- Insert selected list of tags Tags_List to templates
      procedure Insert_Tags (Tags_List : Tags_Container.Map) is
         use AWS.Templates.Utils;
      begin
         Insert_Tags_Loop :
         for I in Tags_List.Iterate loop
            if To_Lower(Item => Tags_List(I)) = "true" then
               Insert
                 (Set => Tags,
                  Item =>
                    Assoc
                      (Variable => Tags_Container.Key(Position => I),
                       Value => True));
            elsif To_Lower(Item => Tags_List(I)) = "false" then
               Insert
                 (Set => Tags,
                  Item =>
                    Assoc
                      (Variable => Tags_Container.Key(Position => I),
                       Value => False));
            else
               if Is_Number(S => Tags_List(I)) then
                  Insert
                    (Set => Tags,
                     Item =>
                       Assoc
                         (Variable => Tags_Container.Key(Position => I),
                          Value => Integer'Value(Tags_List(I))));
               else
                  Insert
                    (Set => Tags,
                     Item =>
                       Assoc
                         (Variable => Tags_Container.Key(Position => I),
                          Value => Tags_List(I)));
               end if;
            end if;
         end loop Insert_Tags_Loop;
      end Insert_Tags;

   begin
      Read_Page_File_Block :
      declare
         use Ada.Characters.Latin_1;

         Data: Unbounded_String := Null_Unbounded_String;
         Start_Index: Natural := 0;
         Start_Pos: constant Positive :=
           Length(Source => Yass_Conf.Markdown_Comment);
         Valid_Value: Boolean := False;

         -------------
         -- Add_Tag --
         -------------

         -- Add tag to the page template tags lists (simple or composite).
         -- Name: name of the tag
         -- Value: value of the tag
         procedure Add_Tag (Name  : String;
                            Value : String)
         is
            use Ada.Calendar;
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

      begin
         -- Read selected markdown file
         Open(File => Page_File, Mode => In_File, Name => File_Name);
         Read_Page_File_Loop :
         while not End_Of_File(File => Page_File) loop
            Data :=
              To_Unbounded_String
                (Source => Encode(Item => Get_Line(File => Page_File)));
            if Length(Source => Data) < 3 then
               Append(Source => Content, New_Item => Data);
               Append(Source => Content, New_Item => LF);
               goto End_Of_Loop;
            end if;
            if Unbounded_Slice(Source => Data, Low => 1, High => Start_Pos) /=
              Yass_Conf.Markdown_Comment then
               Append(Source => Content, New_Item => Data);
               Append(Source => Content, New_Item => LF);
               goto End_Of_Loop;
            end if;
            -- Get the page template layout
            if Index(Source => Data, Pattern => "layout:", From => 1) =
              Start_Pos + 2 then
               Data :=
                 Unbounded_Slice
                   (Source => Data, Low => Start_Pos + 10,
                    High => Length(Source => Data));
               Layout :=
                 Yass_Conf.Layouts_Directory & Dir_Separator & Data &
                 To_Unbounded_String(Source => ".html");
               if not Ada.Directories.Exists
                   (Name => To_String(Source => Layout)) then
                  Close(File => Page_File);
                  raise Layout_Not_Found
                    with File_Name & """. Selected layout file """ &
                    To_String(Source => Layout);
               end if;
               -- Set update frequency for the page in the sitemap
            elsif Index(Source => Data, Pattern => "changefreq:", From => 1) =
              Start_Pos + 2 then
               Change_Frequency :=
                 Unbounded_Slice
                   (Source => Data, Low => Start_Pos + 14,
                    High => Length(Source => Data));
               Validate_Frequency_Loop :
               for Value of Frequency_Values loop
                  if Change_Frequency = Value then
                     Valid_Value := True;
                     exit Validate_Frequency_Loop;
                  end if;
               end loop Validate_Frequency_Loop;
               if not Valid_Value then
                  raise Sitemap_Invalid_Value
                    with "Invalid value for changefreq";
               end if;
               Valid_Value := False;
               -- Set priority for the page in the sitemap
            elsif Index(Source => Data, Pattern => "priority:", From => 1) =
              Start_Pos + 2 then
               Page_Priority :=
                 Unbounded_Slice
                   (Source => Data, Low => Start_Pos + 11,
                    High => Length(Source => Data));
               Validate_Priority_Block :
               begin
                  if Float'Value(To_String(Source => Page_Priority)) < 0.0 or
                    Float'Value(To_String(Source => Page_Priority)) > 1.0 then
                     raise Sitemap_Invalid_Value
                       with "Invalid value for page priority";
                  end if;
               exception
                  when Constraint_Error =>
                     raise Sitemap_Invalid_Value
                       with "Invalid value for page priority";
               end Validate_Priority_Block;
               -- Check if the page is excluded from the sitemap
            elsif Index(Source => Data, Pattern => "insitemap:", From => 1) =
              Start_Pos + 2 then
               if To_Lower
                   (Item =>
                      Slice
                        (Source => Data, Low => Start_Pos + 13,
                         High => Length(Source => Data))) =
                 "false" then
                  In_Sitemap := False;
               end if;
               -- Add tag to the page tags lists
            else
               Start_Index :=
                 Index(Source => Data, Pattern => ":", From => Start_Pos + 2);
               if Start_Index >
                 Index
                   (Source => Data, Pattern => " ", From => Start_Pos + 2) then
                  Start_Index := 0;
               end if;
               if Start_Index > 0 then
                  Add_Tag
                    (Name =>
                       Slice
                         (Source => Data, Low => Start_Pos + 2,
                          High => Start_Index - 1),
                     Value =>
                       Slice
                         (Source => Data, Low => Start_Index + 2,
                          High => Length(Source => Data)));
               end if;
            end if;
            <<End_Of_Loop>>
         end loop Read_Page_File_Loop;
         Close(File => Page_File);
      end Read_Page_File_Block;

      -- Convert markdown to HTML
      Page_Tags.Include
        (Key      => "Content",
         New_Item => CMark.Markdown_To_HTML
                       (Text         => To_String (Content),
                        HTML_Enabled => Yass_Conf.HTML_Enabled));

      -- Load the program modules with 'pre' hook
      Load_Modules
        (State => "pre", Page_Tags => Page_Tags,
         Page_Table_Tags => Page_Table_Tags);
      -- Insert tags to template
      Insert
        (Set => Tags,
         Item => Assoc(Variable => "Content", Value => Page_Tags("Content")));
      Insert_Tags(Tags_List => Site_Tags);
      if not Exists(Set => Tags, Variable => "canonicallink") then
         Insert
           (Set => Tags,
            Item =>
              Assoc
                (Variable => "canonicallink",
                 Value =>
                   To_String(Source => Yass_Conf.Base_Url) & "/" &
                   Slice
                     (Source => To_Unbounded_String(Source => New_File_Name),
                      Low =>
                        Length
                          (Source =>
                             Yass_Conf.Output_Directory & Dir_Separator) +
                        1,
                      High => New_File_Name'Length)));
      end if;
      if not Exists(Set => Tags, Variable => "author") then
         Insert
           (Set => Tags,
            Item =>
              Assoc
                (Variable => "author",
                 Value => To_String(Source => Yass_Conf.Author_Name)));
      end if;
      if not Exists(Set => Tags, Variable => "description")
        and then Site_Tags.Contains(Key => "Description") then
         Insert
           (Set => Tags,
            Item =>
              Assoc
                (Variable => "description",
                 Value => Site_Tags("Description")));
      end if;
      Add_Table_Tags_Loop :
      for I in Page_Table_Tags.Iterate loop
         Insert
           (Set => Tags,
            Item =>
              Assoc
                (Variable => TableTags_Container.Key(Position => I),
                 Value => Page_Table_Tags(I)));
      end loop Add_Table_Tags_Loop;
      Add_Global_Table_Tags_Loop :
      for I in Global_Table_Tags.Iterate loop
         Insert
           (Set => Tags,
            Item =>
              Assoc
                (Variable => TableTags_Container.Key(Position => I),
                 Value => Global_Table_Tags(I)));
      end loop Add_Global_Table_Tags_Loop;
      Insert_Tags(Tags_List => Page_Tags);
      -- Create HTML file in Output_Directory
      Create_Path(New_Directory => To_String(Source => Output_Directory));
      Create(File => Page_File, Mode => Append_File, Name => New_File_Name);

      if Layout = "" then
         raise Layout_Not_Found with To_String (Layout);
      end if;

      Put
        (File => Page_File,
         Item =>
           Decode
             (Item =>
                Parse
                  (Filename => To_String(Source => Layout),
                   Translations => Tags)));
      Close(File => Page_File);
      -- Add the page to the sitemap
      if In_Sitemap then
         Add_Page_To_Sitemap
           (File_Name => New_File_Name,
            Change_Frequency => To_String(Source => Change_Frequency),
            Page_Priority => To_String(Source => Page_Priority));
      end if;
      -- Add the page to the Atom feed
      if Yass_Conf.Atom_Feed_Source =
        To_Unbounded_String(Source => "tags") then
         Atom_Entries(Atom_Entries.First_Index).Content := Content;
      end if;
      Add_Page_To_Feed(File_Name => New_File_Name, Entries => Atom_Entries);
      Ada.Environment_Variables.Set (Name  => "YASSFILE",
                                     Value => New_File_Name);
      -- Load the program modules with 'post' hook
      Load_Modules
        (State => "post", Page_Tags => Page_Tags,
         Page_Table_Tags => Page_Table_Tags);
   exception

      when An_Exception : Layout_Not_Found =>
         Put_Line
           ("Can not parse layout. File """
            & Exception_Message (X => An_Exception)
            & """ does not exist.");
         raise Generate_Site_Exception;

      when An_Exception : Template_Error =>
         Put_Line(Item => Exception_Message(X => An_Exception));
         if Ada.Directories.Exists(Name => New_File_Name) then
            Close(File => Page_File);
            Delete_File(Name => New_File_Name);
         end if;
         raise Generate_Site_Exception;
      when An_Exception : Sitemap_Invalid_Value =>
         Put_Line
           (Item =>
              "Can't parse """ & File_Name & """. " &
              Exception_Message(X => An_Exception));
         raise Generate_Site_Exception;
      when An_Exception : Invalid_Value =>
         Put_Line
           (Item =>
              "Can't parse """ & File_Name & """. Invalid value for tag: " &
              Exception_Message(X => An_Exception));
         raise Generate_Site_Exception;
   end Create_Page;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File (File_Name : String;
                        Directory : String)
   is
      use Ada.Directories;
      use Ada.Strings.Unbounded;

      Output_Directory: constant Unbounded_String :=
        Yass_Conf.Output_Directory &
        Delete
          (Source => To_Unbounded_String(Source => Directory), From => 1,
           Through => Length(Source => Site_Directory));
      Page_Tags: Tags_Container.Map := Tags_Container.Empty_Map;
      Page_Table_Tags: TableTags_Container.Map :=
        TableTags_Container.Empty_Map;
   begin
      -- Load the program modules with 'pre' hook
      Load_Modules
        (State => "pre", Page_Tags => Page_Tags,
         Page_Table_Tags => Page_Table_Tags);
      -- Copy the file to output directory
      Create_Path(New_Directory => To_String(Source => Output_Directory));
      if Ada.Directories.Kind(Name => File_Name) = Ada.Directories.Directory
      then
         Create_Path(New_Directory =>
            To_String(Source => Output_Directory) & Dir_Separator &
               Simple_Name(Name => File_Name));
      else
         Ada.Directories.Copy_File
         (Source_Name => File_Name,
            Target_Name =>
            To_String(Source => Output_Directory) & Dir_Separator &
            Simple_Name(Name => File_Name));
         if Extension(Name => File_Name) = "html" then
            Add_Page_To_Sitemap
            (File_Name =>
               To_String(Source => Output_Directory) & Dir_Separator &
               Simple_Name(Name => File_Name),
               Change_Frequency => "", Page_Priority => "");
         end if;

         Ada.Environment_Variables.Set
           (Name  => "YASSFILE",
            Value =>
              To_String (Output_Directory) & Dir_Separator &
              Simple_Name (File_Name));
      end if;
      -- Load the program modules with 'post' hook
      Load_Modules
        (State => "post", Page_Tags => Page_Tags,
         Page_Table_Tags => Page_Table_Tags);
   end Copy_File;

   -----------------------
   -- Create_Empty_File --
   -----------------------

   procedure Create_Empty_File (File_Name : String)
   is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded;

      Index_File : File_Type;
      Comment    : constant String := To_String (Yass_Conf.Markdown_Comment);

      procedure PL (Item : String) is
      begin
         Put_Line (Index_File, Comment & " " & Item);
      end PL;

   begin
      if Ada.Directories.Extension (Name => File_Name) = "md" then
         Create (File => Index_File,
                 Mode => Append_File,
                 Name => File_Name);
      else
         Create (File => Index_File,
                 Mode => Append_File,
                 Name => File_Name & Dir_Separator & "index.md");
      end if;

      PL ("All lines which starts with double minus sign are comments and ignored");
      PL ("by program. Unless they have colon sign. Then they are tags definition.");
      PL ("Ada Web Server template which will be used as HTML template for this");
      PL ("file. Required for each file");
      PL ("");
      PL ("layout: default");
      PL ("");
      PL ("You may add as many tags as you want, and they can be in any place in");
      PL ("file, not only at beginning. Tags can be 4 types: strings, boolean,");
      PL ("numeric or composite.");
      PL ("First 3 types of tags are in Name: Value scheme. For strings, it can be");
      PL ("any alphanumeric value without new line sign. For boolean it must be");
      PL ("""true"" or ""false"", for numeric any number. Program will detect self");
      PL ("which type of tag is and properly set it. It always falls back to");
      PL ("string value.");
      PL ("Composite tags first must be initialized with Name: [] then just add");
      PL ("as many as you want values to it by Name: Value scheme.");
      PL ("");
      PL ("For more information about tags please check program documentation.");
      PL ("");
      PL ("If you have enabled creation of sitemap in the project config file,");
      PL ("you can set some sitemap parameters too. They are defined in this same");
      PL ("way like tags, with ParameterName: Value.");
      PL ("");
      PL ("priority - The priority of this URL relative to other URLs on your site,");
      PL ("           value between 0.0 and 1.0.");
      PL ("changefreq - How frequently the page is likely to change, value can be");
      PL ("             always, hourly, daily, weekly, monthly, yearly or never.");
      PL ("");
      PL ("For more information how this options works, please look at the program");
      PL ("documentation.");
      PL ("");
      PL ("Additionally, you can exclude this file from adding to sitemap by");
      PL ("setting option insitemap: false.");
      PL ("");
      PL ("If you have enabled creating Atom feed for the site, you must specify");
      PL ("""title"" tag for this page. If you want to use this file as a main");
      PL ("source of Atom feed, then you must add ""title"" tag for each section");
      PL ("which will be used as source for Atom feed entry. If you want to set");
      PL ("author name for Atom feed, you must add ""author"" tag or setting Author");
      PL ("from configuration file will be used. When you want to set author email");
      PL ("for Atom feed, you must add ""authoremail"" tag. If you want to add");
      PL ("short entry summary, you must add tag ""summary"". Do that tag will be");
      PL ("for whole page or for each entry depends on your Atom feed configuration.");
      PL ("");
      PL ("You can also specify canonical link for the page. If you don't set it");
      PL ("here, the program will generate it automatically. To set the default");
      PL ("canonical link for the page set tag ""canonicallink"". It must be a");
      PL ("full URL (with https://).");
      PL ("By setting ""author"" tag for the page, you can overwrite the");
      PL ("configuration setting for meta tag author for the page.");
      PL ("");
      PL ("title: New page");
      PL ("");
      PL ("You can without problem delete all this comments from this file.");

      Close (Index_File);

   end Create_Empty_File;

   ---------------------
   -- Get_Layout_Name --
   ---------------------

   function Get_Layout_Name (File_Name : String) return String
   is
      use Ada.Strings.Unbounded;
      use Ada.Strings.UTF_Encoding.Strings;
      use Ada.Text_IO;

      Page_File: File_Type;
      Data, Layout: Unbounded_String := Null_Unbounded_String;
      Start_Pos: constant Positive :=
        Length(Source => Yass_Conf.Markdown_Comment);
   begin
      Open(File => Page_File, Mode => In_File, Name => File_Name);
      Find_Layout_Name_Loop :
      while not End_Of_File(File => Page_File) loop
         Data :=
           To_Unbounded_String
             (Source => Encode(Item => Get_Line(File => Page_File)));
         if Length(Source => Data) > 2
           and then
             Unbounded_Slice(Source => Data, Low => 1, High => Start_Pos) =
             Yass_Conf.Markdown_Comment
           and then Index(Source => Data, Pattern => "layout:", From => 1) =
             Start_Pos + 2 then
            Data :=
              Unbounded_Slice
                (Source => Data, Low => 12, High => Length(Source => Data));
            Layout :=
              Yass_Conf.Layouts_Directory & Dir_Separator & Data &
              To_Unbounded_String(Source => ".html");
            if not Ada.Directories.Exists
                (Name => To_String(Source => Layout)) then
               Close(File => Page_File);
               raise Layout_Not_Found
                 with File_Name & """. Selected layout file """ &
                 To_String(Source => Layout);
            end if;
            Close(File => Page_File);
            return To_String(Source => Layout);
         end if;
      end loop Find_Layout_Name_Loop;
      Close(File => Page_File);
      return "";
   end Get_Layout_Name;

end Pages;
