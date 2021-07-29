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
            Expect(Module, Result, ".+", 1_000);
            case Result is
               when 1 =>
                  null;
               when Expect_Timeout =>
                  Finished := True;
                  goto End_Of_Loop;
               when others =>
                  goto End_Of_Loop;
            end case;
            Text := To_Unbounded_String(Expect_Out_Match(Module));
            exit Read_Response_Loop when Text = To_Unbounded_String("done");
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
                     Edit_Table_Tag(Global_Table_Tags);
                  when PAGETAG =>
                     Edit_Tag(Page_Tags);
                  when PAGETABLETAG =>
                     Edit_Table_Tag(Page_Table_Tags);
               end case;
            else
               Put_Line(To_String(Text));
            end if;
            <<End_Of_Loop>>
         end loop Read_Response_Loop;
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
