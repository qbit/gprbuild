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

package com.saabgroup.enterprisebus.VALID is

   type VALID is new DDS.Boolean;


   type VALID_Access is access all VALID;
   type VALID_Array is array (Natural range <>) of aliased VALID;
   pragma Convention (C, VALID_Array);

   function Get_TypeCode return DDS.TypeCode_Access;
   pragma Import (C, Get_TypeCode, "com_saabgroup_enterprisebus_VALID_get_typecode");

   procedure Initialize (This : in out VALID);
   procedure Finalize (This : in out VALID);
   procedure Copy (Dst : in out VALID;
                  Src : in VALID);


end com.saabgroup.enterprisebus.VALID;
