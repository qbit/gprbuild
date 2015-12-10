with Test2_Ada_Root;
package Test2_Ada_Model_Base is
   type Model_Base_Type is
     new Test2_Ada_Root.Root_Type with private;
   procedure Execute              (X : in out Model_Base_Type);
   procedure Do_Initialize        (X : in out Model_Base_Type);
   procedure Do_Start_Of_exercise (X : in out Model_Base_Type);
   procedure Do_Freeze            (X : in out Model_Base_Type);
   procedure Do_Run               (X : in out Model_Base_Type);
   procedure Do_End_Of_Exercise   (X : in out Model_Base_Type);
   procedure Do_Shutdown          (X : in out Model_Base_Type);

private
   type State_Type is
     (Initialize,
      Start_Of_Exercise,
      Freeze,
      Run,
      End_Of_Exercise,
      Shutdown);

   for State_Type use
     (Initialize        => 1,
      Start_Of_Exercise => 2,
      Freeze            => 3,   
      Run               => 4,
      End_Of_Exercise   => 5,
      Shutdown          => 6);

   type Model_Base_Type is
     new Test2_Ada_Root.Root_Type with null record;

   pragma Import (C_Plus_Plus, Model_Base_Type);
   pragma Import (C_Plus_Plus, Execute);
   pragma Import (C_Plus_Plus, Do_Initialize);
   pragma Import (C_Plus_Plus, Do_Start_Of_Exercise);
   pragma Import (C_Plus_Plus, Do_Freeze);
   pragma Import (C_Plus_Plus, Do_Run);
   pragma Import (C_Plus_Plus, Do_End_Of_Exercise);
   pragma Import (C_Plus_Plus, Do_Shutdown);
end Test2_Ada_Model_Base;
