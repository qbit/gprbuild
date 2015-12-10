pragma Ada_05;
package body DDS.RootSeqence is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : in out RootSeqence)
   is
   begin
      This.SetInterface (This.Owned'Address);
   end Initialize;

end DDS.RootSeqence;
