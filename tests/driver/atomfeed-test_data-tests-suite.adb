--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body AtomFeed.Test_Data.Tests.Suite is

   use AUnit.Test_Suites;

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.AtomFeed.Test_Data.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_To_Time_e953f5_b6f0f8 : aliased Runner_1.Test_Case;
   Case_2_1_Test_To_HTTP_Date_2bd5b1_0b38f7 : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_To_Time_e953f5_b6f0f8,
         "atomfeed.ads:77:7:",
         Test_To_Time_e953f5_b6f0f8'Access);
      Runner_1.Create
        (Case_2_1_Test_To_HTTP_Date_2bd5b1_0b38f7,
         "atomfeed.ads:91:7:",
         Test_To_HTTP_Date_2bd5b1_0b38f7'Access);

      Add_Test (Result'Access, Case_1_1_Test_To_Time_e953f5_b6f0f8'Access);
      Add_Test (Result'Access, Case_2_1_Test_To_HTTP_Date_2bd5b1_0b38f7'Access);

      return Result'Access;

   end Suite;

end AtomFeed.Test_Data.Tests.Suite;
--  end read only
