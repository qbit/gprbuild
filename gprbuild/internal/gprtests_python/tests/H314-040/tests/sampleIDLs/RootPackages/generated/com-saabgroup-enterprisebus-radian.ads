--  ============================================================================
--
--         WARNING: THIS FILE IS AUTO-GENERATED. DO NOT MODIFY.
--
--  This file was generated from com_saabgroup_enterprisebus.idl using "rtiddsgen".
--  The rtiddsgen tool is part of the RTI Data Distribution Service distribution.
--  For more information, type 'rtiddsgen -help' at a command shell
--  or consult the RTI Data Distribution Service manual.
--
--  ============================================================================

with DDS;

package com.saabgroup.enterprisebus.RADIAN is

   type RADIAN is new DDS.Double;


   type RADIAN_Access is access all RADIAN;
   type RADIAN_Array is array (Natural range <>) of aliased RADIAN;
   pragma Convention (C, RADIAN_Array);

   function Get_TypeCode return DDS.TypeCode_Access;
   pragma Import (C, Get_TypeCode, "com_saabgroup_enterprisebus_RADIAN_get_typecode");

   procedure Initialize (This : in out RADIAN);
   procedure Finalize (This : in out RADIAN);
   procedure Copy (Dst : in out RADIAN;
                  Src : in RADIAN);


end com.saabgroup.enterprisebus.RADIAN;
