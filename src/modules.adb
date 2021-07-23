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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Expect; use GNAT.Expect;
with AWS.Templates; use AWS.Templates;
with Messages; use Messages;

package body Modules is

   procedure Load_Modules
     (State: String; Page_Tags: in out Tags_Container.Map;
      Page_Table_Tags: in out TableTags_Container.Map) is
      -- Run executable file with Item as full path name
      procedure Run_Module(Item: Directory_Entry_Type) is
         Module: Process_Descriptor;
         Finished: Boolean := False;
         Result: Expect_Match := 0;
         Text, Tag_Name: Unbounded_String := Null_Unbounded_String;
         type Tag_Types is
           (NOTAG, GLOBALTAG, GLOBALTABLETAG, PAGETAG, PAGETABLETAG);
         -- Check if tag with selected name exists and return it type
         function Tag_Exist return Tag_Types is
            use Tags_Container;
         begin
            if Site_Tags.Contains(Key => To_String(Source => Tag_Name)) then
               return GLOBALTAG;
            elsif Global_Table_Tags.Contains
                (Key => To_String(Source => Tag_Name)) then
               return GLOBALTABLETAG;
            end if;
            if Page_Tags = Tags_Container.Empty_Map then
               return NOTAG;
            end if;
            if Page_Tags.Contains(Key => To_String(Source => Tag_Name)) then
               return PAGETAG;
            elsif Page_Table_Tags.Contains
                (Key => To_String(Source => Tag_Name)) then
               return PAGETABLETAG;
            end if;
            return NOTAG;
         end Tag_Exist;
         -- Send to the module values for selected composite tag in TableTags list of tags.
         -- First response contains amount of values.
         procedure Send_Table_Tag(Table_Tags: TableTags_Container.Map) is
            Key: constant String := To_String(Source => Tag_Name);
         begin
            Send
              (Descriptor => Module,
               Str => Natural'Image(Size(T => Table_Tags(Key))));
            Send_Table_Tags_Loop :
            for I in 1 .. Size(T => Table_Tags(Key)) loop
               Send
                 (Descriptor => Module,
                  Str => AWS.Templates.Item(T => Table_Tags(Key), N => I));
            end loop Send_Table_Tags_Loop;
         end Send_Table_Tag;
         -- Edit selected simple tag in selected Tags list of tags.
         procedure Edit_Tag(Tags: in out Tags_Container.Map) is
            Key: constant String := To_String(Tag_Name);
         begin
            if Tags_Container.Contains(Tags, Key) then
               Tags_Container.Exclude(Tags, Key);
            end if;
            Tags_Container.Include
              (Tags, Key, Slice(Text, Index(Text, " ", 10) + 1, Length(Text)));
            Send(Module, "Success");
         end Edit_Tag;
   -- Edit selected composite tag in selected TableTags list of composite tags.
         procedure EditTableTag(TableTags: in out TableTags_Container.Map) is
            StartIndex: Positive;
            TagValue, TagIndex: Unbounded_String;
            TempTag: Vector_Tag;
            TableIndex: Integer;
         begin
            StartIndex := Length(Tag_Name) + 9;
            TagIndex :=
              Unbounded_Slice
                (Text, Index(Text, " ", StartIndex) + 1,
                 Index(Text, " ", StartIndex + 1) - 1);
            StartIndex := StartIndex + Length(TagIndex);
            TagValue :=
              Unbounded_Slice
                (Text, Index(Text, " ", StartIndex) + 1, Length(Text));
            TableIndex := Integer'Value(To_String(TagIndex));
            if TableIndex <= Size(TableTags(To_String(Tag_Name))) and
              TableIndex > 0 then
               TempTag := +"";
               for I in 1 .. Size(TableTags(To_String(Tag_Name))) loop
                  if TableIndex = I then
                     TempTag := TempTag & To_String(TagValue);
                  else
                     TempTag :=
                       TempTag &
                       AWS.Templates.Item(TableTags(To_String(Tag_Name)), I);
                  end if;
               end loop;
               TableTags(To_String(Tag_Name)) := TempTag;
               Send(Module, "Success");
               -- Invalid tag index in tags list
            else
               Send
                 (Module,
                  "Index """ & To_String(TagIndex) & """ is not in tag """ &
                  To_String(Tag_Name) & """ index range.");
            end if;
         end EditTableTag;
      begin
         if not Is_Executable_File(Full_Name(Item)) then
            return;
         end if;
         -- Spawn selected module
         Non_Blocking_Spawn
           (Module, Full_Name(Item), Argument_String_To_List("").all);
         while not Finished loop
            -- Wait for the response from the module
            Expect(Module, Result, ".+", 1_000);
            if Result = Expect_Timeout then
               Finished := True;
               goto End_Of_Loop;
            elsif Result > 1 then
               goto End_Of_Loop;
            end if;
            Text := To_Unbounded_String(Expect_Out_Match(Module));
            exit when Text = To_Unbounded_String("done");
            if Length(Text) < 7 then
               Put_Line(To_String(Text));
               goto End_Of_Loop;
            end if;
            -- Send value of selected tag to the module
            if Slice(Text, 1, 6) = "gettag" then
               Tag_Name := Unbounded_Slice(Text, 8, Length(Text));
               case Tag_Exist is
                  when NOTAG =>
                     Send
                       (Module,
                        "Tag with name """ & To_String(Tag_Name) &
                        """ doesn't exists.");
                  when GLOBALTAG =>
                     Send(Module, Site_Tags(To_String(Tag_Name)));
                  when GLOBALTABLETAG =>
                     Send_Table_Tag(Global_Table_Tags);
                  when PAGETAG =>
                     Send(Module, Page_Tags(To_String(Tag_Name)));
                  when PAGETABLETAG =>
                     Send_Table_Tag(Page_Table_Tags);
               end case;
               -- Edit value of selected tag with new value from the module
            elsif Slice(Text, 1, 7) = "Edit_Tag" then
               Tag_Name := Unbounded_Slice(Text, 9, Index(Text, " ", 10) - 1);
               case Tag_Exist is
                  when NOTAG =>
                     Send
                       (Module,
                        "Tag with name """ & To_String(Tag_Name) &
                        """ don't exists.");
                  when GLOBALTAG =>
                     Edit_Tag(Site_Tags);
                  when GLOBALTABLETAG =>
                     EditTableTag(Global_Table_Tags);
                  when PAGETAG =>
                     Edit_Tag(Page_Tags);
                  when PAGETABLETAG =>
                     EditTableTag(Page_Table_Tags);
               end case;
            else
               Put_Line(To_String(Text));
            end if;
            <<End_Of_Loop>>
         end loop;
         Close(Module);
      exception
         when Invalid_Process =>
            Show_Message("Module " & Full_Name(Item) & " failed to execute.");
         when Process_Died =>
            null;
      end Run_Module;
   begin
      if not Exists
          (To_String(Yass_Config.Modules_Directory) & Dir_Separator &
           State) then
         return;
      end if;
      Search
        (To_String(Yass_Config.Modules_Directory) & Dir_Separator & State, "",
         (Directory => False, others => True), Run_Module'Access);
   end Load_Modules;

end Modules;
