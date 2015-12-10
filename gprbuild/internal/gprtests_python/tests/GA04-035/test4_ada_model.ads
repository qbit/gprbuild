with Test4_Ada_Model_Base;

package Test4_Ada_Model is

   type Object_Type is
     new Test4_Ada_Model_Base.Object_Type with null record;
   
   procedure Do_Initialize(X : in out Object_Type);
   procedure Do_Start_Of_Exercise(X : in out Object_Type);
   procedure Do_Freeze(X : in out Object_Type);
   procedure Do_Run(X : in out Object_Type);
   procedure Do_End_Of_Exercise(X : in out Object_Type);
   procedure Do_Shutdown(X : in out Object_Type);
   
end Test4_Ada_Model;
