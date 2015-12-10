pragma Ada_05;

with System;
with Ada.Finalization;

package DDS.Struct is
   type Ref is abstract new Ada.Finalization.Controlled with null record;

   procedure Pull_To_NativeI
     (This : not null access constant Ref;
      Data : System.Address) is abstract;

   procedure Pull_From_NativeI
     (This : not null access constant Ref;
      Data : System.Address) is abstract;

end DDS.Struct;
