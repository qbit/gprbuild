pragma Ada_05;
--  with CORBA.Sequences.Unbounded;
--  pragma Elaborate_All (CORBA.Sequences.Unbounded);
with DDS.StrMapBase;

package DDS.StrMapRelation is

   type Value_Ref is new DDS.StrMapBase.Abstract_Value_Ref with null record;


   type Item is record
      Key : DDS.String;
      Ref : DDS.ObjectReference;
   end record;

--     package IDL_SEQUENCE_DDS_StrMapRelation_Item is
--       new CORBA.Sequences.Unbounded (DDS.StrMapRelation.Item);
--
--     type ItemSeq is new DDS.StrMapRelation.IDL_SEQUENCE_DDS_StrMapRelation_Item.Sequence;


   function Is_Composition (Self : access Value_Ref) return DDS.Boolean;


end DDS.StrMapRelation;
