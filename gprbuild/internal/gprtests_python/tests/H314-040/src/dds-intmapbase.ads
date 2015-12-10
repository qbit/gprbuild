pragma Ada_05;
with DDS.CollectionBase;
package DDS.IntMapBase is
   type Abstract_Value_Ref is new  DDS.CollectionBase.Abstract_Value_Ref with null record;

   procedure Which_Added
     (Self    : in Abstract_Value_Ref;
      Keys    : out DDS.Long_Seq.Sequence;
      Returns : out DDS.Boolean);

   function Get_All_Keys
     (Self : in Abstract_Value_Ref)
      return DDS.Long_Seq.Sequence;


   procedure Remove
     (Self : in Abstract_Value_Ref;
      Key  : in Interfaces.Integer_32);

end DDS.IntMapBase;
