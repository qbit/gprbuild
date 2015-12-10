pragma Ada_05;
package body DDS.QOSPolicy is

   function Constructor (Params : not null access Integer) return Ref is
      Ret : Ref;
   begin
      return Ret;
   end Constructor;
   ---------------------
   -- Pull_To_NativeI --
   ---------------------

   procedure Pull_To_NativeI
     (This : not null access constant Ref;
      Data : System.Address)
   is
   begin
      null;
   end Pull_To_NativeI;

   -----------------------
   -- Pull_From_NativeI --
   -----------------------

   procedure Pull_From_NativeI
     (This : not null access constant Ref;
      Data : System.Address)
   is
   begin
      null;
   end Pull_From_NativeI;

end DDS.QOSPolicy;
