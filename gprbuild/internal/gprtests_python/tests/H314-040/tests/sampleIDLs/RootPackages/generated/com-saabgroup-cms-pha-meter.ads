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

package com.saabgroup.cms.pha.METER is

   type METER is new DDS.Double;


   type METER_Access is access all METER;
   type METER_Array is array (Natural range <>) of aliased METER;
   pragma Convention (C, METER_Array);

   function Get_TypeCode return DDS.TypeCode_Access;
   pragma Import (C, Get_TypeCode, "com_saabgroup_cms_pha_METER_get_typecode");

   procedure Initialize (This : in out METER);
   procedure Finalize (This : in out METER);
   procedure Copy (Dst : in out METER;
                  Src : in METER);


end com.saabgroup.cms.pha.METER;
