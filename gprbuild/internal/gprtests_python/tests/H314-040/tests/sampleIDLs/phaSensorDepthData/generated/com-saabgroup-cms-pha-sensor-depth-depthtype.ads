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
with com.saabgroup.enterprisebus.METER;

package com.saabgroup.cms.pha.sensor.depth.DepthType is

   TypeName : DDS.String := DDS.To_DDS_String  ("DepthType");

   type DepthType is record
    value :  com.saabgroup.enterprisebus.METER.METER;
    error :  com.saabgroup.enterprisebus.METER.METER;
   end record;
   pragma Convention (C, DepthType);

   type DepthType_Access is access all DepthType;
   type DepthType_Array is array (Natural range <>) of aliased DepthType;
   pragma Convention (C, DepthType_Array);

   function Get_TypeCode return DDS.TypeCode_Access;
   pragma Import (C, Get_TypeCode, "com_saabgroup_cms_pha_sensor_depth_DepthType_get_typecode");

   procedure Initialize (This : in out DepthType);
   procedure Finalize (This : in out DepthType);
   procedure Copy (Dst : in out DepthType;
                   Src : in DepthType);

end com.saabgroup.cms.pha.sensor.depth.DepthType;
