with Ada.Text_Io;

package body Test4_Ada_Model_Base is

   procedure Execute(X : in out Object_Type) is
      State : State_Type := State_Type'Val(X.State - 1); --why -1 ??
      Y : Object_Type'class renames Object_Type'class(X);
   begin
      
      Ada.Text_Io.Put_Line("Test4_Ada_Model_Base::Execute called, State = " 
                           & Integer'Image(X.State));
      case State is
         when Initialize =>
            Y.Do_Initialize;
            
         when Start_Of_Exercise =>
            Y.Do_Start_Of_Exercise;
            
         when Freeze =>
            Y.Do_Freeze;
            
         when Run =>
            Y.Do_Run;
            
         when End_Of_Exercise =>
            Y.Do_End_Of_Exercise;
            
         when Shutdown =>
            Y.Do_Shutdown;
            
      end case;
   end Execute;

   procedure Do_Initialize(X : in out Object_Type) is
   begin
      Ada.Text_Io.Put_Line("Test4_Ada_base_model::do_initialize()");
   end Do_Initialize;

   procedure Do_Start_Of_Exercise(X : in out Object_Type) is
   begin
      Ada.Text_Io.Put_Line("Test4_Ada_base_model::Do_Start_Of_Exercise()");
   end Do_Start_Of_Exercise;

   procedure Do_Freeze(X : in out Object_Type) is
   begin
      Ada.Text_Io.Put_Line("Test4_Ada_base_model::Do_Freeze()");
   end Do_Freeze;

   procedure Do_Run(X : in out Object_Type) is
   begin
      Ada.Text_Io.Put_Line("Test4_Ada_base_model::Do_Run()");
   end Do_Run;

   procedure Do_End_Of_Exercise(X : in out Object_Type) is
   begin
      Ada.Text_Io.Put_Line("Test4_Ada_base_model::Do_End_Of_Exercise()");
   end Do_End_Of_Exercise;

   procedure Do_Shutdown(X : in out Object_Type) is
   begin
      Ada.Text_Io.Put_Line("Test4_Ada_base_model::Do_Shutdown()");
   end Do_Shutdown;

   -- function Get_State_Name(X : in Object_Type) return String is
      -- State : State_Type := State_Type'Val(X.State);
   -- begin
      -- return State_Type'Image(State);
   -- end Get_State_Name;
   
end Test4_Ada_Model_Base;
