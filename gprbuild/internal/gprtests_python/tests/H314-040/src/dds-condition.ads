pragma Ada_05;

limited with DDS.Condition_Impl;

package DDS.Condition is

   type Ref is limited interface;
   type Ref_Access is access all Ref'Class;
   type Ref_Access_Access is access all Ref_Access;
   type Ref_Access_Array is array (Natural range <>) of aliased Ref_Access;

   function Get_Trigger_Value
     (Self : access Ref)
     return DDS.Boolean is abstract;

   function Get_Impl_I
     (Self : access Ref)
     return access DDS.Condition_Impl.Ref is abstract;

   procedure Initialize
     (Self : in out Ref_Access) is null;

   procedure Finalize
     (Self : in out Ref_Access) is null;

   procedure Copy
     (Dst : in out Ref_Access;
      Src : in Ref_Access) is null;

end DDS.Condition;
