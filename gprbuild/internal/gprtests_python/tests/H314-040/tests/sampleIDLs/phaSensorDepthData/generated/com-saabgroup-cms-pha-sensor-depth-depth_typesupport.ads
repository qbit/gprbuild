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

with DDS;
with DDS.DomainParticipant;
with DDS.TypeSupport;
with DDS.DataReader;
with DDS.DataWriter;

with com.saabgroup.cms.pha.sensor.depth.Depth; use com.saabgroup.cms.pha.sensor.depth.Depth;

package com.saabgroup.cms.pha.sensor.depth.Depth_TypeSupport is

   type Ref is new DDS.TypeSupport.Ref with null record;
   type Ref_Access is access all Ref'Class;

   function Create_TypedDataReaderI
     (Self : access Ref) return DDS.DataReader.Ref_Access;

   procedure Destroy_TypedDataReaderI
     (Self   : access Ref;
      Reader : in out DDS.DataReader.Ref_Access);

   function Create_TypedDataWriterI
     (Self : access Ref) return DDS.DataWriter.Ref_Access;

   procedure Destroy_TypedDataWriterI
     (Self   : access Ref;
      Writer : in out DDS.DataWriter.Ref_Access);

   --  static methods

   procedure Register_Type
     (Participant :  not null access DDS.DomainParticipant.Ref'Class;
      Type_Name   : in DDS.String);

   function Get_Type_Name return DDS.String;

   function Create_Data (AllocatePointers : in Boolean := True)
     return not null Depth_Access;

   procedure Delete_Data
     (A_Data : in out Depth_Access; DeletePointers : in Boolean := True);

   procedure Print_Data (A_Data : not null access constant Depth.Depth);

   procedure Copy_Data
     (Dest   : not null access Depth.Depth;
      Source : not null access constant Depth.Depth);

   procedure Initialize_Data
     (Dest             : not null access Depth.Depth;
      AllocatePointers : in Boolean := True);

   procedure Finalize_Data
     (Dest           : not null access Depth.Depth;
      DeletePointers : in Boolean := True);

   procedure Finalize;

end com.saabgroup.cms.pha.sensor.depth.Depth_TypeSupport;
