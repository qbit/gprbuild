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



pragma Warnings (Off); --  Since this is auto generated code.
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with DDS;
with DDS.Entity;
with Ada.Finalization;
with System;
pragma Warnings (On);

package com.saabgroup.cms.pha.sensor.depth is

   IDL_VERSION : constant DDS.String := DDS.To_DDS_String ("2.1.0");

   DEFAULT_TOPIC : constant DDS.String := DDS.To_DDS_String ("com.saabgroup.cms.pha.sensor.depth.t_depth_sensors");

end com.saabgroup.cms.pha.sensor.depth;
