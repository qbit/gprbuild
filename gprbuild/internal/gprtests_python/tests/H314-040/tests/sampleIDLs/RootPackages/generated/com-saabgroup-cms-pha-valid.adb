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

with RTI;
package body com.saabgroup.cms.pha.VALID is
   use type RTI.Bool;
   procedure Initialize
     (This              : in out VALID) is
      function Internal
        (This : not null access VALID)
         return RTI.Bool;
      pragma Import (C, Internal, "com_saabgroup_cms_pha_VALID_initialize_ex");
   begin
      if not Internal (This'Unrestricted_Access) then
         raise DDS.ERROR with "unable to initialize";
      end if;
   end Initialize;

   procedure Finalize
     (This            : in out VALID) is
      function Internal
        (This : access VALID)
         return RTI.Bool;
      pragma Import (C, Internal, "com_saabgroup_cms_pha_VALID_finalize_ex");
   begin
      if not Internal (This'Unrestricted_Access) then
         raise DDS.ERROR with "unable to finalize";
      end if;
   end Finalize;

   procedure Copy
     (Dst : in out VALID;
      Src : in VALID) is
      function Internal
        (Dst : not null access VALID;
         Src : not null access VALID)
         return RTI.Bool;
      pragma Import (C, Internal, "com_saabgroup_cms_pha_VALID_copy");
   begin
      if not Internal (Dst'Unrestricted_Access, Src'Unrestricted_Access) then
         raise DDS.ERROR with "unable to copy";
      end if;
   end Copy;

end com.saabgroup.cms.pha.VALID;
