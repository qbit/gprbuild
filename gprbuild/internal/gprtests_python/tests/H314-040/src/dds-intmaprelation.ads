pragma Ada_05;

--  with CORBA.Sequences.Unbounded;
--  pragma Elaborate_All (CORBA.Sequences.Unbounded);
with DDS.IntMapBase;

package DDS.IntMapRelation is

   type Value_Ref is new DDS.IntMapBase.Abstract_Value_Ref with null record;


   type Item is record
      Key : Interfaces.Integer_32;
      Ref : DDS.ObjectReference;
   end record;

--     package IDL_SEQUENCE_DDS_IntMapRelation_Item is
--       new CORBA.Sequences.Unbounded
--         (DDS.IntMapRelation.Item);
--
--     type ItemSeq is
--       new DDS.IntMapRelation.IDL_SEQUENCE_DDS_IntMapRelation_Item.Sequence;


   function Is_Composition
     (Self : access Value_Ref)
      return DDS.Boolean;

end DDS.IntMapRelation;
