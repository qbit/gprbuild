--  ============================================================================
--
--         WARNING: THIS FILE IS AUTO-GENERATED. DO NOT MODIFY.
--
--  This file was generated from timeType.idl using "rtiddsgen".
--  The rtiddsgen tool is part of the RTI Data Distribution Service distribution.
--  For more information, type 'rtiddsgen -help' at a command shell
--  or consult the RTI Data Distribution Service manual.
--
--  ============================================================================

with DDS;
with com.saabgroup.enterprisebus.generalservices.time.qualityType;

package com.saabgroup.enterprisebus.generalservices.time.TimeType is

   TypeName : DDS.String := DDS.To_DDS_String  ("TimeType");

   type TimeType is record
    timeSeconds :  DDS.Unsigned_Long;
    timeFractions :  DDS.Unsigned_Long;
    quality :  com.saabgroup.enterprisebus.generalservices.time.qualityType.qualityType;
   end record;
   pragma Convention (C, TimeType);

   type TimeType_Access is access all TimeType;
   type TimeType_Array is array (Natural range <>) of aliased TimeType;
   pragma Convention (C, TimeType_Array);

   function Get_TypeCode return DDS.TypeCode_Access;
   pragma Import (C, Get_TypeCode, "com_saabgroup_enterprisebus_generalservices_time_TimeType_get_typecode");

   procedure Initialize (This : in out TimeType);
   procedure Finalize (This : in out TimeType);
   procedure Copy (Dst : in out TimeType;
                   Src : in TimeType);

end com.saabgroup.enterprisebus.generalservices.time.TimeType;
