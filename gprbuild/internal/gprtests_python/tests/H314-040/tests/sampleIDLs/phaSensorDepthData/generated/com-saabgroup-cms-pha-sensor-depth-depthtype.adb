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

with RTI;
package body com.saabgroup.cms.pha.sensor.depth.DepthType is
   use type RTI.Bool;
   procedure Initialize
     (This              : in out DepthType) is
      function Internal
        (This : not null access DepthType)
         return RTI.Bool;
      pragma Import (C, Internal, "com_saabgroup_cms_pha_sensor_depth_DepthType_initialize_ex");
   begin
      if not Internal (This'Unrestricted_Access) then
         raise DDS.ERROR with "unable to initialize";
      end if;
   end Initialize;

   procedure Finalize
     (This            : in out DepthType) is
      function Internal
        (This : access DepthType)
         return RTI.Bool;
      pragma Import (C, Internal, "com_saabgroup_cms_pha_sensor_depth_DepthType_finalize_ex");
   begin
      if not Internal (This'Unrestricted_Access) then
         raise DDS.ERROR with "unable to finalize";
      end if;
   end Finalize;

   procedure Copy
     (Dst : in out DepthType;
      Src : in DepthType) is
      function Internal
        (Dst : not null access DepthType;
         Src : not null access DepthType)
         return RTI.Bool;
      pragma Import (C, Internal, "com_saabgroup_cms_pha_sensor_depth_DepthType_copy");
   begin
      if not Internal (Dst'Unrestricted_Access, Src'Unrestricted_Access) then
         raise DDS.ERROR with "unable to copy";
      end if;
   end Copy;

end com.saabgroup.cms.pha.sensor.depth.DepthType;
