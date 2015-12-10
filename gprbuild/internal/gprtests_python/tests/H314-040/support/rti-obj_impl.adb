pragma Ada_05;

with System; use System;

package body RTI.Obj_Impl is

   function GetInterface
     (Self : not null access Ref)
     return System.Address
   is
   begin
      return Self.Data;
   end GetInterface;

   ------------------
   -- SetInterface --
   ------------------

   procedure SetInterface
     (Self :  not null access Ref;
      To   : System.Address)
   is
   begin
      Self.Data := To;
   end SetInterface;

   -----------------------
   -- Interface_Is_Null --
   -----------------------

   function Interface_Is_Null
     (Self :  not null access Ref)
     return Boolean
   is
   begin
      return  (Self.Data = System.Null_Address);
   end Interface_Is_Null;

   procedure Free (Self : in out Ref_Access) is
   begin
      Free_Ref (Self);
   end Free;


end RTI.Obj_Impl;

