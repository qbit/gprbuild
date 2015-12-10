pragma Ada_05;
with DDS.CollectionBase;
package DDS.ListBase is

   type Abstract_Value_Ref is new  DDS.CollectionBase.Abstract_Value_Ref with null record;

   procedure Remove
     (Self : in Abstract_Value_Ref);

end DDS.ListBase;
