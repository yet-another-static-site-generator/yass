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

with Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Expect;
with AWS.Templates;
with Messages;

package body Modules is

   procedure Load_Modules
     (State: String; Page_Tags: in out Tags_Container.Map;
      Page_Table_Tags: in out TableTags_Container.Map) is
      use Ada.Strings.Unbounded;
      use Ada.Directories;
      use GNAT.Directory_Operations;

      -- Run executable file with Item as full path name
      procedure Run_Module(Item: Directory_Entry_Type) is
         use Ada.Text_IO;
         use GNAT.OS_Lib;
         use GNAT.Expect;
         use AWS.Templates;
         use Messages;

         Module: Process_Descriptor;
         Finished: Boolean := False;
         Result: Expect_Match := 0;
         Text, Tag_Name: Unbounded_String := Null_Unbounded_String;
         type Tag_Types is
           (NOTAG, GLOBALTAG, GLOBALTABLETAG, PAGETAG, PAGETABLETAG) with
            Default_Value => NOTAG;
         Empty_Tag: constant Tag_Types := NOTAG;
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
               return Empty_Tag;
            end if;
            if Page_Tags.Contains(Key => To_String(Source => Tag_Name)) then
               return PAGETAG;
            elsif Page_Table_Tags.Contains
                (Key => To_String(Source => Tag_Name)) then
               return PAGETABLETAG;
            end if;
            return Empty_Tag;
         end Tag_Exist;
         -- Send to the module values for selected composite tag in Table_Tags list of tags.
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
            Key: constant String := To_String(Source => Tag_Name);
         begin
            if Tags.Contains(Key => Key) then
               Tags.Exclude(Key => Key);
            end if;
            Tags.Include
              (Key => Key,
               New_Item =>
                 Slice
                   (Source => Text,
                    Low =>
                      Index(Source => Text, Pattern => " ", From => 10) + 1,
                    High => Length(Source => Text)));
            Send(Descriptor => Module, Str => "Success");
         end Edit_Tag;
   -- Edit selected composite tag in selected Table_Tags list of composite tags.
         procedure Edit_Table_Tag
           (Table_Tags: in out TableTags_Container.Map) is
            Start_Index: Positive;
            Tag_Value, Tag_Index: Unbounded_String;
            Temp_Tag: Vector_Tag; --## rule line off IMPROPER_INITIALIZATION
            Table_Index: Integer;
         begin
            Start_Index := Length(Source => Tag_Name) + 9;
            Tag_Index :=
              Unbounded_Slice
                (Source => Text,
                 Low =>
                   Index(Source => Text, Pattern => " ", From => Start_Index) +
                   1,
                 High =>
                   Index
                     (Source => Text, Pattern => " ",
                      From => Start_Index + 1) -
                   1);
            --## rule off ASSIGNMENTS
            Start_Index := Start_Index + Length(Source => Tag_Index);
            --## rule on ASSIGNMENTS
            Tag_Value :=
              Unbounded_Slice
                (Source => Text,
                 Low =>
                   Index(Source => Text, Pattern => " ", From => Start_Index) +
                   1,
                 High => Length(Source => Text));
            Table_Index := Integer'Value(To_String(Source => Tag_Index));
            if Table_Index <=
              Size(T => Table_Tags(To_String(Source => Tag_Name))) and
              Table_Index > 0 then
               Temp_Tag := +"";
               Set_Tags_Loop :
               for I in
                 1 .. Size(T => Table_Tags(To_String(Source => Tag_Name))) loop
                  if Table_Index = I then
                     Temp_Tag := Temp_Tag & To_String(Source => Tag_Value);
                  else
                     Temp_Tag :=
                       Temp_Tag &
                       AWS.Templates.Item
                         (T => Table_Tags(To_String(Source => Tag_Name)),
                          N => I);
                  end if;
               end loop Set_Tags_Loop;
               Table_Tags(To_String(Source => Tag_Name)) := Temp_Tag;
               Send(Descriptor => Module, Str => "Success");
               -- Invalid tag index in tags list
            else
               Send
                 (Descriptor => Module,
                  Str =>
                    "Index """ & To_String(Source => Tag_Index) &
                    """ is not in tag """ & To_String(Source => Tag_Name) &
                    """ index range.");
            end if;
         end Edit_Table_Tag;
      begin
         if not Is_Executable_File
             (Name => Full_Name(Directory_Entry => Item)) then
            return;
         end if;
         -- Spawn selected module
         Non_Blocking_Spawn
           (Descriptor => Module,
            Command => Full_Name(Directory_Entry => Item),
            Args => Argument_String_To_List(Arg_String => "").all);
         Read_Response_Loop :
         while not Finished loop
            -- Wait for the response from the module
            Expect
              (Descriptor => Module, Result => Result, Regexp => ".+",
               Timeout => 1_000);
            case Result is
               when 1 =>
                  null;
               when Expect_Timeout =>
                  Finished := True;
                  goto End_Of_Loop;
               when others =>
                  goto End_Of_Loop;
            end case;
            Text :=
              To_Unbounded_String
                (Source => Expect_Out_Match(Descriptor => Module));
            exit Read_Response_Loop when Text =
              To_Unbounded_String(Source => "done");
            if Length(Source => Text) < 7 then
               Put_Line(Item => To_String(Source => Text));
               goto End_Of_Loop;
            end if;
            -- Send value of selected tag to the module
            if Slice(Source => Text, Low => 1, High => 6) = "gettag" then
               Tag_Name :=
                 Unbounded_Slice
                   (Source => Text, Low => 8, High => Length(Source => Text));
               case Tag_Exist is
                  when Empty_Tag =>
                     Send
                       (Descriptor => Module,
                        Str =>
                          "Tag with name """ & To_String(Source => Tag_Name) &
                          """ doesn't exists.");
                  when GLOBALTAG =>
                     Send
                       (Descriptor => Module,
                        Str => Site_Tags(To_String(Source => Tag_Name)));
                  when GLOBALTABLETAG =>
                     Send_Table_Tag(Table_Tags => Global_Table_Tags);
                  when PAGETAG =>
                     Send
                       (Descriptor => Module,
                        Str => Page_Tags(To_String(Source => Tag_Name)));
                  when PAGETABLETAG =>
                     Send_Table_Tag(Table_Tags => Page_Table_Tags);
               end case;
               -- Edit value of selected tag with new value from the module
            elsif Slice(Source => Text, Low => 1, High => 7) = "Edit_Tag" then
               Tag_Name :=
                 Unbounded_Slice
                   (Source => Text, Low => 9,
                    High =>
                      Index(Source => Text, Pattern => " ", From => 10) - 1);
               case Tag_Exist is
                  when Empty_Tag =>
                     Send
                       (Descriptor => Module,
                        Str =>
                          "Tag with name """ & To_String(Source => Tag_Name) &
                          """ don't exists.");
                  when GLOBALTAG =>
                     Edit_Tag(Tags => Site_Tags);
                  when GLOBALTABLETAG =>
                     Edit_Table_Tag(Table_Tags => Global_Table_Tags);
                  when PAGETAG =>
                     Edit_Tag(Tags => Page_Tags);
                  when PAGETABLETAG =>
                     Edit_Table_Tag(Table_Tags => Page_Table_Tags);
               end case;
            else
               Put_Line(Item => To_String(Source => Text));
            end if;
            <<End_Of_Loop>>
         end loop Read_Response_Loop;
         Close(Descriptor => Module);
      exception
         when Invalid_Process =>
            Show_Message
              (Text =>
                 "Module " & Full_Name(Directory_Entry => Item) &
                 " failed to execute.");
         when Process_Died =>
            null;
      end Run_Module;
   begin
      if not Exists
          (Name =>
             To_String(Source => Yass_Config.Modules_Directory) &
             Dir_Separator & State) then
         return;
      end if;
      Search
        (Directory =>
           To_String(Source => Yass_Config.Modules_Directory) & Dir_Separator &
           State,
         Pattern => "", Filter => (Directory => False, others => True),
         Process => Run_Module'Access);
   end Load_Modules;

end Modules;
