pragma Ada_05;
package body DDS.StrMapBase is

   -----------------
   -- Which_Added --
   -----------------

   procedure Which_Added
     (Self    : in Abstract_Value_Ref;
      Keys    : out DDS.String_Seq.Sequence;
      Returns : out DDS.Boolean)
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
   end Which_Added;

   ------------------
   -- Get_All_Keys --
   ------------------

   function Get_All_Keys
     (Self : in Abstract_Value_Ref)
      return DDS.String_Seq.Sequence
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
      return Get_All_Keys (Self);
   end Get_All_Keys;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self : in Abstract_Value_Ref;
      Key  : in DDS.String)
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
   end Remove;

end DDS.StrMapBase;
