with Test2_Ada_Model_Base;
package Test2_Ada_Model is
   type Model_Type is
     new Test2_Ada_Model_Base.Model_Base_Type with private;
   
   procedure Do_Initialize        (X : in out Model_Type);
   procedure Do_Start_Of_Exercise (X : in out Model_Type);
   procedure Do_Freeze            (X : in out Model_Type);
   procedure Do_Run               (X : in out Model_Type);
   procedure Do_End_Of_Exercise   (X : in out Model_Type);
   procedure Do_Shutdown          (X : in out Model_Type);

private
   type Model_Type is
     new Test2_Ada_Model_Base.Model_Base_Type with null record;
   pragma Import (C_Plus_Plus, Model_Type);

   type Class_Ptr_Type is access all Model_Type'class;

   procedure Test_Dispatching (Ptr : Class_Ptr_Type);

   function Constructor return Model_Type'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import(C_Plus_Plus, Constructor, "_ZN15Test2_Cpp_modelC2Ev");

   function Create return Class_Ptr_Type;
   pragma Export(C, Create, "create2_ada_model");

   pragma Import (C_Plus_Plus, Do_Initialize);
   pragma Import (C_Plus_Plus, Do_Start_Of_Exercise);
   pragma Import (C_Plus_Plus, Do_Freeze);
   pragma Import (C_Plus_Plus, Do_Run);
   pragma Import (C_Plus_Plus, Do_End_Of_Exercise);
   pragma Import (C_Plus_Plus, Do_Shutdown);
end Test2_Ada_Model;
