--  ============================================================================
--
--         WARNING: THIS FILE IS AUTO-GENERATED. DO NOT MODIFY.
--
--  This file was generated from com_saabgroup_cms_pha.idl using "rtiddsgen".
--  The rtiddsgen tool is part of the RTI Data Distribution Service distribution.
--  For more information, type 'rtiddsgen -help' at a command shell
--  or consult the RTI Data Distribution Service manual.
--
--  ============================================================================

with DDS;

package com.saabgroup.cms.pha.METER_PER_SECOND is

   type METER_PER_SECOND is new DDS.Double;


   type METER_PER_SECOND_Access is access all METER_PER_SECOND;
   type METER_PER_SECOND_Array is array (Natural range <>) of aliased METER_PER_SECOND;
   pragma Convention (C, METER_PER_SECOND_Array);

   function Get_TypeCode return DDS.TypeCode_Access;
   pragma Import (C, Get_TypeCode, "com_saabgroup_cms_pha_METER_PER_SECOND_get_typecode");

   procedure Initialize (This : in out METER_PER_SECOND);
   procedure Finalize (This : in out METER_PER_SECOND);
   procedure Copy (Dst : in out METER_PER_SECOND;
                  Src : in METER_PER_SECOND);


end com.saabgroup.cms.pha.METER_PER_SECOND;
