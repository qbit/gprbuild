pragma Ada_05;

with System;

package body DDS.ReadCondition_Impl is

   function Get_Trigger_Value
     (Self : access Ref)
     return DDS.Boolean
   is
   begin
      return False;
   end Get_Trigger_Value;

   function Get_Sample_State_Mask
     (Self : not null access Ref)
     return DDS.SampleStateMask
   is
   begin
      return 0;
   end Get_Sample_State_Mask;

   function Get_View_State_Mask
     (Self : not null access Ref)
     return DDS.ViewStateMask
   is
   begin
      return 0;
   end Get_View_State_Mask;

   function Get_Instance_State_Mask
     (Self : not null access Ref)
     return DDS.InstanceStateMask
   is
   begin
      return 0;
   end Get_Instance_State_Mask;

   function Get_DataReader
     (Self : not null access Ref)
     return access DDS.DataReader.Ref'Class
   is
   begin
      return null;
   end Get_DataReader;

   function Get_Impl_I
     (Self : access Ref)
      return access DDS.Condition_Impl.Ref
   is
   begin
      return DDS.Condition_Impl.Ref_Access (Self);
   end Get_Impl_I;

end DDS.ReadCondition_Impl;
