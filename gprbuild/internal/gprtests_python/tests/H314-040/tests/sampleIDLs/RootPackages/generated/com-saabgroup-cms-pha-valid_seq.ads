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

with DDS.Sequences_Generic;
with com.saabgroup.cms.pha.VALID; use com.saabgroup.cms.pha.VALID;

package com.saabgroup.cms.pha.VALID_Seq is new DDS.Sequences_Generic
  (com.saabgroup.cms.pha.VALID.VALID,
   com.saabgroup.cms.pha.VALID.VALID_Access,
   Natural,
   1,
   com.saabgroup.cms.pha.VALID.VALID_Array);

