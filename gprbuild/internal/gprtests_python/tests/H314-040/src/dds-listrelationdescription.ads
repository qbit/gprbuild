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

with DDS.RelationDescription;

package DDS.ListRelationDescription is

   type Value_Ref is new DDS.RelationDescription.Value_Ref with null record;

   Repository_Id : constant Standard.String
     := "IDL:DDS/ListRelationDescription:1.0";


   function get_index
     (Self : access Value_ref)
     return Interfaces.Integer_32;

   procedure set_index
     (Self : access Value_ref;
      To : in Interfaces.Integer_32);

--     function get_kind
--       (Self : access Value_ref)
--       return DDS.RelationKind;
   --  Implicitly inherited from DDS.RelationDescription

--     procedure set_kind
--       (Self : access Value_ref;
--        To : in DDS.RelationKind);
   --  Implicitly inherited from DDS.RelationDescription

--     function get_name
--       (Self : access Value_ref)
--       return DDS.RelationName;
   --  Implicitly inherited from DDS.RelationDescription

--     procedure set_name
--       (Self : access Value_ref;
--        To : in DDS.RelationName);
   --  Implicitly inherited from DDS.RelationDescription

end DDS.ListRelationDescription;
