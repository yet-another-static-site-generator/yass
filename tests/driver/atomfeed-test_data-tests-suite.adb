--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body AtomFeed.Test_Data.Tests.Suite is

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.AtomFeed.Test_Data.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_To_Time_e953f5 : aliased Runner_1.Test_Case;
   Case_2_1_Test_To_HTTP_Date_2bd5b1 : aliased Runner_1.Test_Case;
   Case_3_1_Test_StartAtomFeed_57b23e : aliased Runner_1.Test_Case;
   Case_4_1_Test_AddPageToFeed_6edf8c : aliased Runner_1.Test_Case;
   Case_5_1_Test_SaveAtomFeed_b0372c : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_To_Time_e953f5,
         "atomfeed.ads:36:4:",
         Test_To_Time_e953f5'Access);
      Runner_1.Create
        (Case_2_1_Test_To_HTTP_Date_2bd5b1,
         "atomfeed.ads:38:4:",
         Test_To_HTTP_Date_2bd5b1'Access);
      Runner_1.Create
        (Case_3_1_Test_StartAtomFeed_57b23e,
         "atomfeed.ads:40:4:",
         Test_StartAtomFeed_57b23e'Access);
      Runner_1.Create
        (Case_4_1_Test_AddPageToFeed_6edf8c,
         "atomfeed.ads:41:4:",
         Test_AddPageToFeed_6edf8c'Access);
      Runner_1.Create
        (Case_5_1_Test_SaveAtomFeed_b0372c,
         "atomfeed.ads:44:4:",
         Test_SaveAtomFeed_b0372c'Access);

      Result.Add_Test (Case_1_1_Test_To_Time_e953f5'Access);
      Result.Add_Test (Case_2_1_Test_To_HTTP_Date_2bd5b1'Access);
      Result.Add_Test (Case_3_1_Test_StartAtomFeed_57b23e'Access);
      Result.Add_Test (Case_4_1_Test_AddPageToFeed_6edf8c'Access);
      Result.Add_Test (Case_5_1_Test_SaveAtomFeed_b0372c'Access);

      return Result'Access;

   end Suite;

end AtomFeed.Test_Data.Tests.Suite;
--  end read only
