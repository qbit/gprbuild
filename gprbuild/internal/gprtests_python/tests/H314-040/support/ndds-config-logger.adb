pragma Ada_05;
with System;
package body NDDS.Config.Logger is

   ------------------
   -- Get_Instance --
   ------------------
   The_Logger : aliased Ref;

   procedure Initialize is
      function Internal return System.Address;
      pragma Import (C, Internal, "NDDS_Config_Logger_get_instance");
   begin
      The_Logger.SetInterface (Internal);
   end Initialize;

   function Get_Instance return access Ref is
   begin
      return The_Logger'Unchecked_Access;
   end Get_Instance;

   -------------------------------
   -- Set_Verbosity_By_Category --
   -------------------------------

   procedure Set_Verbosity_By_Category
     (This      : not null access Ref;
      Category  : in NDDS.Config.LOG_CATEGORY;
      Verbosity : in NDDS.Config.Log_Verbosity)
   is
      procedure Internal (Self      : in System.Address;
                          Category  : in NDDS.Config.LOG_CATEGORY;
                          Verbosity : in NDDS.Config.Log_Verbosity);
      pragma Import (C, Internal, "NDDS_Config_Logger_set_verbosity_by_category");
   begin
      Internal (This.GetInterface,
                Category,
                Verbosity);
   end Set_Verbosity_By_Category;

begin
   Initialize;
end NDDS.Config.Logger;
