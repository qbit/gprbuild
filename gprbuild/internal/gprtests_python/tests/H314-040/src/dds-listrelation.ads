pragma Ada_05;

with  DDS.ListBase;
package DDS.ListRelation is

   type Value_Ref is new DDS.ListBase.Abstract_Value_Ref with null record;

   function Is_Composition
     (Self : access Value_Ref)
      return DDS.Boolean;


end DDS.ListRelation;
