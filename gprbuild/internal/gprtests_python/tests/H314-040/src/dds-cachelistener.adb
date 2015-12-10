pragma Ada_05;
package body DDS.CacheListener is

   -------------------
   -- begin_updates --
   -------------------
   function Constructor (Params : not null access Integer) return Ref is
      Ret : Ref;
   begin
      return Ret;
   end Constructor;

   procedure begin_updates
     (Self : in Ref;
      update_round : in Interfaces.Integer_32)
   is
   begin
      null;
   end begin_updates;

   -----------------
   -- end_updates --
   -----------------

   procedure end_updates
     (Self : in Ref;
      update_round : in Interfaces.Integer_32)
   is
   begin
      null;
   end end_updates;

end DDS.CacheListener;
