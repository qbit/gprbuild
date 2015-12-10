with Ada.Finalization; use Ada.Finalization;
package My_ctrl is
   type State is (uninit, init, final);
   type T is new Controlled with
      record
         S : State := uninit;
      end record;

   procedure Initialize ( X : in out T);
   procedure Finalize ( X : in out T);

   procedure Show_Status (X : String; O : T);
end My_Ctrl;
