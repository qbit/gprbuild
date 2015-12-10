with Ada.Text_Io;

package body Test4_Ada_Model is
   function Create return Test4_Ada_Model_Base.Class_Ptr_Type;
   pragma Export(C, Create, "create4_ada_model");
   
   function Create return Test4_Ada_Model_Base.Class_Ptr_Type is
   begin
      return new Object_Type;
   end;

   procedure Do_Initialize(X : in out Object_Type) is
   begin
      Ada.Text_Io.Put_Line("Test4_Ada_model::Do_Initialize");
   end Do_Initialize;

   procedure Do_Start_Of_Exercise(X : in out Object_Type) is
   begin
      Ada.Text_Io.Put_Line("Test4_Ada_model::Do_Start_Of_Exercise");
   end Do_Start_Of_Exercise;

   procedure Do_Freeze(X : in out Object_Type) is
   begin
      Ada.Text_Io.Put_Line("Test4_Ada_model::Do_Freeze");
   end Do_Freeze;

   procedure Do_Run(X : in out Object_Type) is
   begin
      Ada.Text_Io.Put_Line("Test4_Ada_model::Do_Run");
   end Do_Run;

   procedure Do_End_Of_Exercise(X : in out Object_Type) is
   begin
      Ada.Text_Io.Put_Line("Test4_Ada_model::Do_End_Of_Exercise");
   end Do_End_Of_Exercise;

   procedure Do_Shutdown(X : in out Object_Type) is
   begin
      Ada.Text_Io.Put_Line("Test4_Ada_model::Do_Shutdown");
   end Do_Shutdown;

end Test4_Ada_Model;
