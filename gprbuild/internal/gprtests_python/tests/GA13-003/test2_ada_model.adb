with Ada.Text_IO; use Ada.Text_IO;

package body Test2_Ada_Model is
   --  Here you have two versions of function Create. They provide the
   --  same functionality but the currently active one calls Test_Dispatching
   --  to verify that the allocated object has been initialized well.
   
--   function Create return Class_Ptr_Type is
--   begin
--      return new Model_Type;
--   end;

   function Create return Class_Ptr_Type is
      Obj  : Class_Ptr_Type;
   begin
      Put_Line ("  Ada_Model::Create (allocating CPP object)");
      Obj := new Model_Type;

      --  Test that all dispatching primitives defined in the C++
      --  side have been properly imported.
      Test_Dispatching (Obj);

      return Obj;
   end;

   procedure Test_Dispatching (Ptr : Class_Ptr_Type) is
   begin
      New_Line;
      Put_Line (">>> Ada_Model_Base.Test_Dispatching"); 
      Put (" Testing Do_Initialize ..........");
      Do_Initialize (Ptr.all);

      Put (" Testing Do_Start_Of_Exercise ...");
      Do_Start_Of_Exercise (Ptr.all);

      Put (" Testing Do_Freeze ..............");
      Do_Freeze (Ptr.all);

      Put (" Testing Do_Run .................");
      Do_Run (Ptr.all);

      Put (" Testing Do_End_Of_Exercise .....");
      Do_End_Of_Exercise (Ptr.all);

      Put (" Testing Do_Shutdown ............");
      Do_Shutdown (Ptr.all);

      Put_Line (" Testing Execute");
      for J in 1 .. 6 loop
          Put ("   "); Ptr.State := J; Execute (Ptr.all);
      end loop;

      Put_Line (">>> Ada_Model_Base.Test_Dispatching (end of test)"); 
      New_Line;
   end Test_Dispatching;
end Test2_Ada_Model;
