pragma Ada_05;

with System;
with RTI.Obj_Impl;

package DDS.RootSeqence is
   type RootSeqence is abstract new RTI.Obj_Impl.Ref with record
      Owned                     : Boolean;
      Contiguous_Buffer         : System.Address;
      Discontiguous_Buffer      : System.Address;
      Maximum                   : DDS.Natural;
      Length                    : DDS.unsigned_long;
      Sequence_Init             : DDS.long;
      Read_Token1               : System.Address;
      Read_Token2               : System.Address;
      ElementPointersAllocation : DDS.Boolean;
   end record;
   pragma Convention (C, RootSeqence);
   overriding
   procedure Initialize (This : in out RootSeqence);
end DDS.RootSeqence;
