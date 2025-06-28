--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
pragma Ada_2005;
with AUnit.Reporter.gnattest;
with AUnit.Run;
with AUnit.Options; use AUnit.Options;
with Gnattest_Main_Suite; use Gnattest_Main_Suite;

with AUnit; use AUnit;
with Ada.Command_Line;
with AUnit.IO;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Gnattest_Generated.Mapping;
with Gnattest_Generated.Persistent;
with GNAT.Command_Line; use GNAT.Command_Line;

with Gnattest_Generated;

procedure Test_Runner is
   function Runner is new AUnit.Run.Test_Runner_With_Status (Suite);
   Exit_Status : AUnit.Status;
   Use_Exit_Status : Boolean := True;
   Reporter : AUnit.Reporter.gnattest.gnattest_Reporter;
   GT_Options : AUnit_Options := Default_Options;
   Fil : aliased Gnattest_Generated.Mapping.GT_Filter;
   SLOC_Found : Boolean;
   procedure Process_Routines (File : String; SLOC_Found : out Boolean);
   procedure Process_Routines (File : String; SLOC_Found : out Boolean) is
      use Ada.Text_IO;
      F : File_Type;
   begin
      Open (F, In_File, GNAT.OS_Lib.Normalize_Pathname (File));
      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
         begin
            if Line'Length < 2 or else (Line (Line'First .. Line'First + 1)) /= "--" then
               Gnattest_Generated.Mapping.Set_From_SLOC (Fil, Line, SLOC_Found);
               if not SLOC_Found then
                     AUnit.IO.Put_Line
                        (AUnit.IO.Standard_Output.all,
                         "no subprogram corresponds to sloc " & Line & "; aborting");
                  return;
               end if;
            end if;
         end;
      end loop;
      Close (F);
   exception
      when others =>
         AUnit.IO.Put_Line
            (AUnit.IO.Standard_Output.all,
             "error opening " & GNAT.OS_Lib.Normalize_Pathname (File));
            raise;
   end Process_Routines;
begin

   GT_Options.Report_Successes := True;

   begin
      Initialize_Option_Scan;
      loop
         case GNAT.Command_Line.Getopt
           ("-skeleton-default= -passed-tests= -exit-status= -routines=")
         is
            when ASCII.NUL =>
               exit;
            when '-' =>
               if Full_Switch = "-skeleton-default" then
                  if Parameter = "pass" then
                     Gnattest_Generated.Default_Assert_Value := True;
                  elsif Parameter = "fail" then
                     Gnattest_Generated.Default_Assert_Value := False;
                  end if;
               end if;
               if Full_Switch = "-routines" then
                  Gnattest_Generated.Mapping.Set_Selection_Mode (Fil);
                  if Parameter (Parameter'First) = '@' then
                     Process_Routines (Parameter (Parameter'First + 1 .. Parameter'Last), SLOC_Found);
                  else
                     Gnattest_Generated.Mapping.Set_From_SLOC (Fil, Parameter, SLOC_Found);
                     if not SLOC_Found then
                        AUnit.IO.Put_Line
                           (AUnit.IO.Standard_Output.all,
                            "no subprogram corresponds to sloc " & Parameter & "; aborting");
                     end if;
                  end if;
                  if not SLOC_Found then
                     return;
                  end if;
               end if;
               if Full_Switch = "-passed-tests" then
                  if Parameter = "show" then
                     GT_Options.Report_Successes := True;
                  elsif Parameter = "hide" then
                     GT_Options.Report_Successes := False;
                  end if;
               end if;
               if Full_Switch = "-exit-status" then
                  if Parameter = "on" then
                     Use_Exit_Status := True;
                  elsif Parameter = "off" then
                     Use_Exit_Status := False;
                  end if;
               end if;
            when others => null;
         end case;
      end loop;
   exception
      when GNAT.Command_Line.Invalid_Switch => null;
   end;

   GT_Options.Filter := Fil'Unchecked_Access;
   Gnattest_Generated.Persistent.Global_Set_Up;
   Exit_Status := Runner (Reporter, GT_Options);
   Gnattest_Generated.Persistent.Global_Tear_Down;
   if Use_Exit_Status and then Exit_Status = AUnit.Failure then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Test_Runner;
--  end read only
