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

with RTI;
package body com.saabgroup.enterprisebus.generalservices.time.TimeType is
   use type RTI.Bool;
   procedure Initialize
     (This              : in out TimeType) is
      function Internal
        (This : not null access TimeType)
         return RTI.Bool;
      pragma Import (C, Internal, "com_saabgroup_enterprisebus_generalservices_time_TimeType_initialize_ex");
   begin
      if not Internal (This'Unrestricted_Access) then
         raise DDS.ERROR with "unable to initialize";
      end if;
   end Initialize;

   procedure Finalize
     (This            : in out TimeType) is
      function Internal
        (This : access TimeType)
         return RTI.Bool;
      pragma Import (C, Internal, "com_saabgroup_enterprisebus_generalservices_time_TimeType_finalize_ex");
   begin
      if not Internal (This'Unrestricted_Access) then
         raise DDS.ERROR with "unable to finalize";
      end if;
   end Finalize;

   procedure Copy
     (Dst : in out TimeType;
      Src : in TimeType) is
      function Internal
        (Dst : not null access TimeType;
         Src : not null access TimeType)
         return RTI.Bool;
      pragma Import (C, Internal, "com_saabgroup_enterprisebus_generalservices_time_TimeType_copy");
   begin
      if not Internal (Dst'Unrestricted_Access, Src'Unrestricted_Access) then
         raise DDS.ERROR with "unable to copy";
      end if;
   end Copy;

end com.saabgroup.enterprisebus.generalservices.time.TimeType;
