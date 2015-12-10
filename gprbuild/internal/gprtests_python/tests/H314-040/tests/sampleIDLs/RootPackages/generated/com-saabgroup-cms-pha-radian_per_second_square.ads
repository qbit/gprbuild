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

package com.saabgroup.cms.pha.RADIAN_PER_SECOND_SQUARE is

   type RADIAN_PER_SECOND_SQUARE is new DDS.Double;


   type RADIAN_PER_SECOND_SQUARE_Access is access all RADIAN_PER_SECOND_SQUARE;
   type RADIAN_PER_SECOND_SQUARE_Array is array (Natural range <>) of aliased RADIAN_PER_SECOND_SQUARE;
   pragma Convention (C, RADIAN_PER_SECOND_SQUARE_Array);

   function Get_TypeCode return DDS.TypeCode_Access;
   pragma Import (C, Get_TypeCode, "com_saabgroup_cms_pha_RADIAN_PER_SECOND_SQUARE_get_typecode");

   procedure Initialize (This : in out RADIAN_PER_SECOND_SQUARE);
   procedure Finalize (This : in out RADIAN_PER_SECOND_SQUARE);
   procedure Copy (Dst : in out RADIAN_PER_SECOND_SQUARE;
                  Src : in RADIAN_PER_SECOND_SQUARE);


end com.saabgroup.cms.pha.RADIAN_PER_SECOND_SQUARE;
