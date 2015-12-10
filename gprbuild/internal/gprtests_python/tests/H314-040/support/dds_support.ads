pragma Ada_05;

with Interfaces;
with Interfaces.C.Extensions;

with RTI;

package DDS_Support is

   subtype Void_Ptr is Interfaces.C.Extensions.void_ptr;

   type InstanceHandle_T is private;

   Null_InstanceHandle_T : constant InstanceHandle_T;

   --
   --  From dds_c_common.h
   --
   type Builtin_Topic_Key_Type_Native is new Interfaces.Unsigned_32;
   Builtin_Topic_Key_Type_Native_INITIALIZER : constant
     Builtin_Topic_Key_Type_Native := 0;

private

   type Guid_T_Prefix is record
      HostId   : Interfaces.Unsigned_32;
      AppId    : Interfaces.Unsigned_32;
      ObjectId : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C, Guid_T_Prefix);

   type Guid_T is record
      Prefix    : Guid_T_Prefix;
      Object_Id : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C, Guid_T);

   type InstanceHandle_T is record
      Guid : Guid_T;
      IsValid : RTI.Bool;
   end record;
   pragma Convention (C, InstanceHandle_T);

   Null_InstanceHandle_T : constant InstanceHandle_T :=
                             (Guid => ((Prefix => (0, 0, 0), Object_Id => 0)),
                              isValid => RTI.False);

end DDS_Support;
