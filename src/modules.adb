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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Expect; use GNAT.Expect;
with AWS.Templates; use AWS.Templates;

package body Modules is

   procedure LoadModules(State: String; PageTags: in out Tags_Container.Map;
      PageTableTags: in out TableTags_Container.Map) is
      procedure RunModule(Item: Directory_Entry_Type) is
         Module: Process_Descriptor;
         Finished: Boolean := False;
         Result: Expect_Match;
         Text, TagName, TagValue, TagIndex: Unbounded_String;
         StartIndex: Positive;
         TempTag: Vector_Tag;
         type Tag_Types is
           (NoTag, GlobalTag, GlobalTableTag, PageTag, PageTableTag);
         function TagExist return Tag_Types is
            use Tags_Container;
         begin
            if Contains(SiteTags, To_String(TagName)) then
               return GlobalTag;
            elsif TableTags_Container.Contains
                (GlobalTableTags, To_String(TagName)) then
               return GlobalTableTag;
            end if;
            if PageTags /= SiteTags then
               if Contains(PageTags, To_String(TagName)) then
                  return PageTag;
               elsif TableTags_Container.Contains
                   (PageTableTags, To_String(TagName)) then
                  return PageTableTag;
               end if;
            end if;
            return NoTag;
         end TagExist;
      begin
         if not Is_Executable_File(Full_Name(Item)) then
            return;
         end if;
         Non_Blocking_Spawn
           (Module, Full_Name(Item), Argument_String_To_List("").all);
         while not Finished loop
            Expect(Module, Result, ".+", 1_000);
            case Result is
               when Expect_Timeout =>
                  Finished := True;
               when 1 =>
                  Text := To_Unbounded_String(Expect_Out_Match(Module));
                  exit when Text = To_Unbounded_String("done");
                  if Length(Text) > 6 then
                     if Slice(Text, 1, 6) = "gettag" then
                        TagName := Unbounded_Slice(Text, 8, Length(Text));
                        case TagExist is
                           when NoTag =>
                              Send
                                (Module,
                                 "Tag with name """ & To_String(TagName) &
                                 """ doesn't exists.");
                           when GlobalTag =>
                              Send(Module, SiteTags(To_String(TagName)));
                           when GlobalTableTag =>
                              Send
                                (Module,
                                 Natural'Image
                                   (Size
                                      (GlobalTableTags(To_String(TagName)))));
                              for I in
                                1 ..
                                  Size
                                    (GlobalTableTags(To_String(TagName))) loop
                                 Send
                                   (Module,
                                    AWS.Templates.Item
                                      (GlobalTableTags(To_String(TagName)),
                                       I));
                              end loop;
                           when PageTag =>
                              Send(Module, PageTags(To_String(TagName)));
                           when PageTableTag =>
                              Send
                                (Module,
                                 Natural'Image
                                   (Size(PageTableTags(To_String(TagName)))));
                              for I in
                                1 ..
                                  Size(PageTableTags(To_String(TagName))) loop
                                 Send
                                   (Module,
                                    AWS.Templates.Item
                                      (PageTableTags(To_String(TagName)), I));
                              end loop;
                        end case;
                     elsif Slice(Text, 1, 7) = "edittag" then
                        TagName :=
                          Unbounded_Slice(Text, 9, Index(Text, " ", 10) - 1);
                        case TagExist is
                           when NoTag =>
                              Send
                                (Module,
                                 "Tag with name """ & To_String(TagName) &
                                 """ don't exists.");
                           when GlobalTag =>
                              TagValue :=
                                Unbounded_Slice
                                  (Text, Index(Text, " ", 10) + 1,
                                   Length(Text));
                              SiteTags(To_String(TagName)) :=
                                To_String(TagValue);
                              Send(Module, "Success");
                           when GlobalTableTag =>
                              StartIndex := Length(TagName) + 9;
                              TagIndex :=
                                Unbounded_Slice
                                  (Text, Index(Text, " ", StartIndex) + 1,
                                   Index(Text, " ", StartIndex + 1) - 1);
                              StartIndex := StartIndex + Length(TagIndex);
                              TagValue :=
                                Unbounded_Slice
                                  (Text, Index(Text, " ", StartIndex) + 1,
                                   Length(Text));
                              if Integer'Value(To_String(TagIndex)) <=
                                Size(GlobalTableTags(To_String(TagName))) and
                                Integer'Value(To_String(TagIndex)) > 0 then
                                 TempTag := +"";
                                 for I in
                                   1 ..
                                     Size
                                       (GlobalTableTags
                                          (To_String(TagName))) loop
                                    if Integer'Value(To_String(TagIndex)) =
                                      I then
                                       TempTag :=
                                         TempTag & To_String(TagValue);
                                    else
                                       TempTag :=
                                         TempTag &
                                         AWS.Templates.Item
                                           (GlobalTableTags
                                              (To_String(TagName)),
                                            I);
                                    end if;
                                 end loop;
                                 GlobalTableTags(To_String(TagName)) :=
                                   TempTag;
                                 Send(Module, "Success");
                              else
                                 Send
                                   (Module,
                                    "Index """ & To_String(TagIndex) &
                                    """ is not in tag """ &
                                    To_String(TagName) & """ index range.");
                              end if;
                           when PageTag =>
                              TagValue :=
                                Unbounded_Slice
                                  (Text, Index(Text, " ", 10) + 1,
                                   Length(Text));
                              PageTags(To_String(TagName)) :=
                                To_String(TagValue);
                              Send(Module, "Success");
                           when PageTableTag =>
                              StartIndex := Length(TagName) + 9;
                              TagIndex :=
                                Unbounded_Slice
                                  (Text, Index(Text, " ", StartIndex) + 1,
                                   Index(Text, " ", StartIndex + 1) - 1);
                              StartIndex := StartIndex + Length(TagIndex);
                              TagValue :=
                                Unbounded_Slice
                                  (Text, Index(Text, " ", StartIndex) + 1,
                                   Length(Text));
                              if Integer'Value(To_String(TagIndex)) <=
                                Size(PageTableTags(To_String(TagName))) and
                                Integer'Value(To_String(TagIndex)) > 0 then
                                 TempTag := +"";
                                 for I in
                                   1 ..
                                     Size
                                       (PageTableTags(To_String(TagName))) loop
                                    if Integer'Value(To_String(TagIndex)) =
                                      I then
                                       TempTag :=
                                         TempTag & To_String(TagValue);
                                    else
                                       TempTag :=
                                         TempTag &
                                         AWS.Templates.Item
                                           (PageTableTags(To_String(TagName)),
                                            I);
                                    end if;
                                 end loop;
                                 PageTableTags(To_String(TagName)) := TempTag;
                                 Send(Module, "Success");
                              else
                                 Send
                                   (Module,
                                    "Index """ & To_String(TagIndex) &
                                    """ is not in tag """ &
                                    To_String(TagName) & """ index range.");
                              end if;
                        end case;
                     else
                        Put_Line(To_String(Text));
                     end if;
                  else
                     Put_Line(To_String(Text));
                  end if;
               when others =>
                  null;
            end case;
         end loop;
         Close(Module);
      exception
         when Invalid_Process =>
            Put_Line("Module " & Full_Name(Item) & " failed to execute.");
         when Process_Died =>
            null;
      end RunModule;
   begin
      if not Exists
          (To_String(YassConfig.ModulesDirectory) & Dir_Separator & State) then
         return;
      end if;
      Search
        (To_String(YassConfig.ModulesDirectory) & Dir_Separator & State, "",
         (Directory => False, others => True), RunModule'Access);
   end LoadModules;

end Modules;
