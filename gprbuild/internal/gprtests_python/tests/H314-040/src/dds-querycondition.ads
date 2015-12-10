pragma Ada_05;
with DDS.ReadCondition;

package DDS.QueryCondition is

   type Ref is interface and DDS.ReadCondition.Ref;
   type Ref_Access is access all Ref'Class;

   function Get_Query_Expression
     (Self : access Ref)
     return DDS.String is abstract;

   procedure Get_Query_Parameters
     (Self : access Ref;
      Params : out DDS.String_Seq.Sequence) is abstract;

   procedure Set_Query_Parameters
     (Self : access Ref;
      Params : DDS.String_Seq.Sequence) is abstract;

end DDS.QueryCondition;
