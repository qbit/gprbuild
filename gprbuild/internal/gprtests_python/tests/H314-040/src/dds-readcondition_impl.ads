pragma Ada_05;

limited with DDS.DataReader;
with DDS.Condition_Impl;
with DDS.ReadCondition;

package DDS.ReadCondition_Impl is

   type Ref is new DDS.Condition_Impl.Ref and DDS.ReadCondition.Ref with null record;
   type Ref_Access is access all Ref'Class;

   function Get_Trigger_Value
     (Self : access Ref)
     return DDS.Boolean;

   function Get_Sample_State_Mask
     (Self : not null access Ref)
     return DDS.SampleStateMask;

   function Get_View_State_Mask
     (Self : not null access Ref)
     return DDS.ViewStateMask;

   function Get_Instance_State_Mask
     (Self : not null access Ref)
     return DDS.InstanceStateMask;

   function Get_DataReader
     (Self : not null access Ref)
      return access DDS.DataReader.Ref'Class;

   function Get_Impl_I
     (Self : access Ref)
     return access DDS.Condition_Impl.Ref;

end DDS.ReadCondition_Impl;
