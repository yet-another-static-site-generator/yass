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

   Case_1_1_Test_To_Time_e953f5_c5180a : aliased Runner_1.Test_Case;
   Case_2_1_Test_To_HTTP_Date_2bd5b1_79d01d : aliased Runner_1.Test_Case;
   Case_3_1_Test_Start_Atom_Feed_2493ea_0ccbf2 : aliased Runner_1.Test_Case;
   Case_4_1_Test_Add_Page_To_Feed_467d32_dc8147 : aliased Runner_1.Test_Case;
   Case_5_1_Test_Save_Atom_Feed_73b303_2e8e31 : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_To_Time_e953f5_c5180a,
         "atomfeed.ads:79:7:",
         Test_To_Time_e953f5_c5180a'Access);
      Runner_1.Create
        (Case_2_1_Test_To_HTTP_Date_2bd5b1_79d01d,
         "atomfeed.ads:93:7:",
         Test_To_HTTP_Date_2bd5b1_79d01d'Access);
      Runner_1.Create
        (Case_3_1_Test_Start_Atom_Feed_2493ea_0ccbf2,
         "atomfeed.ads:101:7:",
         Test_Start_Atom_Feed_2493ea_0ccbf2'Access);
      Runner_1.Create
        (Case_4_1_Test_Add_Page_To_Feed_467d32_dc8147,
         "atomfeed.ads:114:7:",
         Test_Add_Page_To_Feed_467d32_dc8147'Access);
      Runner_1.Create
        (Case_5_1_Test_Save_Atom_Feed_73b303_2e8e31,
         "atomfeed.ads:122:7:",
         Test_Save_Atom_Feed_73b303_2e8e31'Access);

      Add_Test (Result'Access, Case_1_1_Test_To_Time_e953f5_c5180a'Access);
      Add_Test (Result'Access, Case_2_1_Test_To_HTTP_Date_2bd5b1_79d01d'Access);
      Add_Test (Result'Access, Case_3_1_Test_Start_Atom_Feed_2493ea_0ccbf2'Access);
      Add_Test (Result'Access, Case_4_1_Test_Add_Page_To_Feed_467d32_dc8147'Access);
      Add_Test (Result'Access, Case_5_1_Test_Save_Atom_Feed_73b303_2e8e31'Access);

      return Result'Access;

   end Suite;

end AtomFeed.Test_Data.Tests.Suite;
--  end read only
