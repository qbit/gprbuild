pragma Ada_05;
package body DDS.IntMapBase is

   -----------------
   -- which_added --
   -----------------

   procedure Which_Added
     (Self    : in Abstract_Value_Ref;
      Keys    : out DDS.Long_Seq.Sequence;
      Returns : out DDS.Boolean)
   is
   begin
      null;
   end Which_Added;

   ------------------
   -- get_all_keys --
   ------------------

   function Get_All_Keys
     (Self : in Abstract_Value_Ref)
      return DDS.Long_Seq.Sequence
   is
   begin
      return Get_All_Keys (Self);
   end Get_All_Keys;

   ------------
   -- remove --
   ------------

   procedure Remove
     (Self : in Abstract_Value_Ref;
      Key  : in Interfaces.Integer_32)
   is
   begin
      null;
   end Remove;

end DDS.IntMapBase;
