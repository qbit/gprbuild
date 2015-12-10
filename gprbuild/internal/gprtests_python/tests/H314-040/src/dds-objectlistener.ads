pragma Ada_05;
-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks ("NM32766");

with DDS.Object;

package DDS.ObjectListener is

   type Ref is new DDS.Object.Ref with null record;

   function on_object_created
     (Self : in Ref;
      ref : in DDS.ObjectReference)
     return DDS.Boolean;

   function on_object_deleted
     (Self : in Ref;
      ref : in DDS.ObjectReference)
     return DDS.Boolean;

end DDS.ObjectListener;
