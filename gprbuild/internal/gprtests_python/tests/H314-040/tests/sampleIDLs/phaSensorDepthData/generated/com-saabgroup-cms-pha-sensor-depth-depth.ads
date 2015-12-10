--  ============================================================================
--
--         WARNING: THIS FILE IS AUTO-GENERATED. DO NOT MODIFY.
--
--  This file was generated from phaSensorDepthData.idl using "rtiddsgen".
--  The rtiddsgen tool is part of the RTI Data Distribution Service distribution.
--  For more information, type 'rtiddsgen -help' at a command shell
--  or consult the RTI Data Distribution Service manual.
--
--  ============================================================================

with DDS;
with com.saabgroup.enterprisebus.Generalservices.Time.TimeType;
with com.saabgroup.enterprisebus.VALID;
with com.saabgroup.cms.pha.Sensor.Depth.DepthType;

package com.saabgroup.cms.pha.Sensor.Depth.Depth is

   TypeName : DDS.String := DDS.To_DDS_String  ("Depth");

   type Depth is record
      SenseTime :  com.saabgroup.enterprisebus.Generalservices.Time.TimeType.TimeType;
      Sensor    :  DDS.Short;
      IsValid   :  com.saabgroup.enterprisebus.VALID.VALID;
      Depth     :  com.saabgroup.cms.pha.Sensor.Depth.DepthType.DepthType;
   end record;
   pragma Convention (C, Depth);

   type Depth_Access is access all Depth;
   type Depth_Array is array (Natural range <>) of aliased Depth;
   pragma Convention (C, Depth_Array);

   function Get_TypeCode return DDS.TypeCode_Access;
   pragma Import (C, Get_TypeCode, "com_saabgroup_cms_pha_sensor_depth_Depth_get_typecode");

   procedure Initialize (This : in out Depth);
   procedure Finalize (This : in out Depth);
   procedure Copy (Dst : in out Depth;
                   Src : in Depth);

end com.saabgroup.cms.pha.Sensor.Depth.Depth;
