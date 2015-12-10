pragma Ada_05;
with System;
with DDS.Struct;
with DDS.Vectors;
package DDS.AbstractQosPolicyContainer is
   type Ref is new DDS.Struct.Ref with record
      Members         : DDS.Vectors.Vector;
      NextMemberIndex : Integer;
   end record;
   procedure Pull_To_NativeI
     (This : not null access constant Ref;
      Data : System.Address);

   procedure Pull_From_NativeI
     (This : not null access constant Ref;
      Data : System.Address);

   function Constructor (Params : not null access Integer) return Ref;
end DDS.AbstractQosPolicyContainer;
