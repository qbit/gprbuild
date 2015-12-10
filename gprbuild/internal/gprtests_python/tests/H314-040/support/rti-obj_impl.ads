pragma Ada_05;

with System;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;

package RTI.Obj_Impl is
   pragma Preelaborate;

   type Ref is new Ada.Finalization.Controlled with private;
   type Ref_Access is access all Ref'Class;

   function GetInterface (Self : not null access Ref)
                         return System.Address;

   procedure SetInterface (Self : not null access Ref;
                           To : System.Address);

   function Interface_Is_Null (Self : not null access Ref)
                              return Boolean;

   procedure Free (Self : in out Ref_Access);

private

   type Ref is new Ada.Finalization.Controlled  with record
      Data  : System.Address;
   end record;

   procedure Free_Ref is new Ada.Unchecked_Deallocation (Ref'Class, Ref_Access);

end RTI.Obj_Impl;
