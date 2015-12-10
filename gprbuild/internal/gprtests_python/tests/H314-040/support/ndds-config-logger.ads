pragma Ada_05;
with RTI.Obj_Impl;
package NDDS.Config.Logger is

   type Ref is new RTI.Obj_Impl.Ref with null record;
   function  Get_Instance return access Ref;
   procedure Set_Verbosity_By_Category (This      : not null access Ref;
                                        Category  : in NDDS.Config.LOG_CATEGORY;
                                        Verbosity : in NDDS.Config.Log_Verbosity);

private
   procedure Initialize;
end NDDS.Config.Logger;
