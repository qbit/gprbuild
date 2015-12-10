with Test4_Ada_Root;

package Test4_Ada_Model_Base is
    type State_Type is (Initialize,
                        Start_Of_Exercise,
                        Freeze,
                        Run,
                        End_Of_Exercise,
                        Shutdown
                       );
   for State_Type use  (Initialize        => 1,
                        Start_Of_Exercise => 2,
                        Freeze            => 3,   
                        Run               => 4,
                        End_Of_Exercise   => 5,
                        Shutdown          => 6
                       );

   type Object_Type is
     new Test4_Ada_Root.Object_Type with null record;
   procedure Execute(X : in out Object_Type);
   
   procedure Do_Initialize(X : in out Object_Type);
   procedure Do_Start_Of_exercise(X : in out Object_Type);
   procedure Do_Freeze(X : in out Object_Type);
   procedure Do_Run(X : in out Object_Type);
   procedure Do_End_Of_Exercise(X : in out Object_Type);
   procedure Do_Shutdown(X : in out Object_Type);
   type Class_Ptr_Type is access all Object_Type'class;


   -- function Get_State_Name(X : in Object_Type) return String;

end Test4_Ada_Model_Base;
