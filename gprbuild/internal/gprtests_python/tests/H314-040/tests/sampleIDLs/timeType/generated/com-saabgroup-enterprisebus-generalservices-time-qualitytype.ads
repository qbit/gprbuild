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

package com.saabgroup.enterprisebus.generalservices.time.qualityType is

   type qualityType is new DDS.Unsigned_Long;


   type qualityType_Access is access all qualityType;
   type qualityType_Array is array (Natural range <>) of aliased qualityType;
   pragma Convention (C, qualityType_Array);

   function Get_TypeCode return DDS.TypeCode_Access;
   pragma Import (C, Get_TypeCode, "com_saabgroup_enterprisebus_generalservices_time_qualityType_get_typecode");

   procedure Initialize (This : in out qualityType);
   procedure Finalize (This : in out qualityType);
   procedure Copy (Dst : in out qualityType;
                  Src : in qualityType);


end com.saabgroup.enterprisebus.generalservices.time.qualityType;
