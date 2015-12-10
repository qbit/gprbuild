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

with System; use System;

with Unchecked_Conversion;

with com.saabgroup.cms.pha.sensor.depth.Depth_DataReader;
with com.saabgroup.cms.pha.sensor.depth.Depth_DataWriter;
with DDS.DomainParticipant_Impl;

package body com.saabgroup.cms.pha.sensor.depth.Depth_TypeSupport is

   C_DataTypeUtility_Ptr : System.Address := System.Null_Address;
   The_Instance : aliased Ref;
   Instance_Ptr : Ref_Access;

   function Create_TypedDataReaderI
     (Self : access Ref) return DDS.DataReader.Ref_Access is
   begin
      return com.saabgroup.cms.pha.sensor.depth.Depth_DataReader.CreateTypedI;
   end  Create_TypedDataReaderI;

   procedure Destroy_TypedDataReaderI
     (Self   : access Ref;
      Reader : in out DDS.DataReader.Ref_Access) is
   begin
      com.saabgroup.cms.pha.sensor.depth.Depth_DataReader.DestroyTypedI (Reader);
   end  Destroy_TypedDataReaderI;

   function Create_TypedDataWriterI
     (Self : access Ref) return DDS.DataWriter.Ref_Access is
   begin
      return com.saabgroup.cms.pha.sensor.depth.Depth_DataWriter.CreateTypedI;
   end  Create_TypedDataWriterI;

   procedure Destroy_TypedDataWriterI
     (Self   : access Ref;
      Writer : in out DDS.DataWriter.Ref_Access) is
   begin
      Writer := null;
   end Destroy_TypedDataWriterI;

   function Get_Native_Typesupport_Ptr
     (DeleteInstance : DDS.Boolean
)
                 return System.Address;
   pragma Import (C, Get_Native_Typesupport_Ptr,"com_saabgroup_cms_pha_sensor_depth_DepthTypeSupport_get_or_delete_instanceI");

   function R_To_A is new Unchecked_Conversion
     (Source => Ref_Access,
      Target => System.Address);

   procedure Set_User_Data
     (Self : System.Address; User_Data : System.Address);
   pragma Import (C, Set_User_Data,                  "DDS_DataTypeUtility_set_user_dataI");

   procedure Initialize is
   begin

      if Instance_Ptr = null then
         Instance_Ptr := The_Instance'Access
;
                     C_DataTypeUtility_Ptr := Get_Native_TypeSupport_Ptr (False);
         if C_DataTypeUtility_Ptr = System.Null_Address then
            DDS.Ret_Code_To_Exception
              (DDS.RETCODE_ERROR,
               "unable to initialize TypeSupport");
         end if;
         Set_User_Data (C_DataTypeUtility_Ptr, R_To_A (Instance_Ptr));
      end if;

   end Initialize;

   -------------------
   -- Register_Type --
   -------------------

   procedure Register_Type
     (Participant : not null access DDS.DomainParticipant.Ref'Class;
      Type_Name   : in DDS.String) is

      P : constant DDS.DomainParticipant_Impl.Ref_Access :=
            DDS.DomainParticipant_Impl.Ref_Access (Participant);

      function Internal
        (Participant : System.Address;
         Type_Name   : in DDS.String)
         return DDS.ReturnCode_T;
      pragma Import (C, Internal, "com_saabgroup_cms_pha_sensor_depth_DepthTypeSupport_register_type");
   begin
      DDS.Ret_Code_To_Exception
        (Internal (P.GetInterface, Type_Name),
         "unable to register type");

      Initialize;

   end Register_Type;

   -------------------
   -- Get_Type_Name --
   -------------------

   function Get_Type_Name
     return DDS.String is
      function Internal return DDS.String;
      pragma Import (C, Internal, "com_saabgroup_cms_pha_sensor_depth_DepthTypeSupport_get_type_name");
   begin
      return Internal;
   end Get_Type_Name;

   -----------------
   -- Create_Data --
   -----------------

   function Create_Data
     (AllocatePointers : in Boolean := True)
      return not null Depth_Access
   is
      function Internal (AllocatePointers : in Boolean)
                         return Depth_Access;
      pragma Import (C, Internal, "com_saabgroup_cms_pha_sensor_depth_DepthTypeSupport_create_data_ex");
      Ret : Depth_Access;
   begin
      Ret := Internal (AllocatePointers);
      if Ret = null then
         raise Storage_Error with "Unable to create " & "Depth";
      else
         return Ret;
      end if;
   end Create_Data;

   -----------------
   -- Delete_Data --
   -----------------

   procedure Delete_Data
     (A_Data         : in out Depth_Access;
      DeletePointers : in Boolean := True)
   is
      function Internal (A_Data         : in Depth_Access;
                         DeletePointers : in Boolean := True)
                         return DDS.ReturnCode_T;
      pragma Import (C, Internal, "com_saabgroup_cms_pha_sensor_depth_DepthTypeSupport_delete_data_ex");
   begin
      DDS.Ret_Code_To_Exception
        (Internal (A_Data, DeletePointers),
         "Unable to delete data");
      A_Data := null;
   end Delete_Data;

   ----------------
   -- Print_Data --
   ----------------

   procedure Print_Data
     (A_Data : not null access constant Depth.Depth)
   is
      procedure Internal (A_Data : not null access constant Depth.Depth);
      pragma Import (C, Internal, "com_saabgroup_cms_pha_sensor_depth_DepthTypeSupport_print_data");
   begin
      Internal (A_Data);
   end Print_Data;

   ---------------
   -- Copy_Data --
   ---------------

   procedure Copy_Data
     (Dest   : not null access Depth.Depth;
      Source : not null access constant Depth.Depth)
   is
      function Internal (Dest   : not null access Depth.Depth;
                         Source : not null access constant Depth.Depth)
                         return DDS.ReturnCode_T;
      pragma Import (C, Internal, "com_saabgroup_cms_pha_sensor_depth_DepthTypeSupport_copy_data");
   begin
      DDS.Ret_Code_To_Exception
        (Internal (Dest, Source),
         "Unable to copy data");
   end Copy_Data;

   ---------------------
   -- Initialize_Data --
   ---------------------

   procedure Initialize_Data
     (Dest             : not null access Depth.Depth;
      AllocatePointers : in Boolean := True)
   is
      function Internal (Dest             : not null access Depth.Depth;
                         AllocatePointers : in Boolean := True)
                         return DDS.ReturnCode_T;
      pragma Import (C, Internal, "com_saabgroup_cms_pha_sensor_depth_DepthTypeSupport_initialize_data_ex");
   begin
      DDS.Ret_Code_To_Exception
        (Internal (Dest, AllocatePointers),
         "Unable to initialize data");
   end Initialize_Data;

   -------------------
   -- Finalize_Data --
   -------------------

   procedure Finalize_Data
     (Dest           : not null access Depth.Depth;
      DeletePointers : in Boolean := True)
   is
      function Internal (Dest           : not null access Depth.Depth;
                         DeletePointers : in Boolean := True)
                         return DDS.ReturnCode_T;
      pragma Import (C, Internal, "com_saabgroup_cms_pha_sensor_depth_DepthTypeSupport_finalize_data_ex");
   begin
      DDS.Ret_Code_To_Exception
        (Internal (Dest, DeletePointers),
         "Unable to finalize data");
   end Finalize_Data;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      function Internal return DDS.ReturnCode_T;
      pragma Import (C, Internal, "com_saabgroup_cms_pha_sensor_depth_DepthTypeSupport_finalize");
   begin
      DDS.Ret_Code_To_Exception (Internal, "Unable to finalize");
   end Finalize;

end com.saabgroup.cms.pha.sensor.depth.Depth_TypeSupport;
