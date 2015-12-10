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

with DDS.Sequences_Generic;
with com.saabgroup.cms.pha.sensor.depth.Depth; use com.saabgroup.cms.pha.sensor.depth.Depth;

package com.saabgroup.cms.pha.sensor.depth.Depth_Seq is new DDS.Sequences_Generic
  (com.saabgroup.cms.pha.sensor.depth.Depth.Depth,
   com.saabgroup.cms.pha.sensor.depth.Depth.Depth_Access,
   Natural,
   1,
   com.saabgroup.cms.pha.sensor.depth.Depth.Depth_Array);

