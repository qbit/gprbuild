pragma Ada_05;

with DDS.Condition;
limited with DDS.DataReader;

package DDS.ReadCondition is

   type Ref is interface and DDS.Condition.Ref;
   type Ref_Access is access all Ref'Class;

   function Get_Sample_State_Mask
     (Self : not null access Ref)
     return DDS.SampleStateMask is abstract;

   function Get_View_State_Mask
     (Self : not null access Ref)
     return DDS.ViewStateMask is abstract;

   function Get_Instance_State_Mask
     (Self : not null access Ref)
     return DDS.InstanceStateMask is abstract;

   function Get_DataReader
     (Self : not null access Ref)
     return access DDS.DataReader.Ref'Class is abstract;

end DDS.ReadCondition;
