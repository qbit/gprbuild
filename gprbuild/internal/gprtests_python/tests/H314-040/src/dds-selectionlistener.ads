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

package DDS.SelectionListener is

   type Ref is new DDS.Object.Ref with null record;

   procedure on_object_out
     (Self : in Ref;
      the_ref : in DDS.ObjectReference);

   on_object_out_Repository_Id : constant Standard.String
     := "IDL:DDS/SelectionListener/on_object_out:1.0";

   Repository_Id : constant Standard.String
     := "IDL:DDS/SelectionListener:1.0";

end DDS.SelectionListener;
