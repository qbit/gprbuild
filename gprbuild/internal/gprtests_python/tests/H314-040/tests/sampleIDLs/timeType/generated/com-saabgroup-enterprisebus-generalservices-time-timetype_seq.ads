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

with DDS.Sequences_Generic;
with com.saabgroup.enterprisebus.generalservices.time.TimeType; use com.saabgroup.enterprisebus.generalservices.time.TimeType;

package com.saabgroup.enterprisebus.generalservices.time.TimeType_Seq is new DDS.Sequences_Generic
  (com.saabgroup.enterprisebus.generalservices.time.TimeType.TimeType,
   com.saabgroup.enterprisebus.generalservices.time.TimeType.TimeType_Access,
   Natural,
   1,
   com.saabgroup.enterprisebus.generalservices.time.TimeType.TimeType_Array);

