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

with DDS.Sequences_Generic;
with com.saabgroup.enterprisebus.METER; use com.saabgroup.enterprisebus.METER;

package com.saabgroup.enterprisebus.METER_Seq is new DDS.Sequences_Generic
  (com.saabgroup.enterprisebus.METER.METER,
   com.saabgroup.enterprisebus.METER.METER_Access,
   Natural,
   1,
   com.saabgroup.enterprisebus.METER.METER_Array);

