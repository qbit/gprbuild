pragma Ada_05;
package body DDS.ObjectListener is

   -----------------------
   -- on_object_created --
   -----------------------

   function on_object_created
     (Self : in Ref;
      ref : in DDS.ObjectReference)
      return DDS.Boolean
   is
   begin
      return on_object_created (Self, ref);
   end on_object_created;

   -----------------------
   -- on_object_deleted --
   -----------------------

   function on_object_deleted
     (Self : in Ref;
      ref : in DDS.ObjectReference)
      return DDS.Boolean
   is
   begin
      return on_object_deleted (Self, ref);
   end on_object_deleted;

end DDS.ObjectListener;
