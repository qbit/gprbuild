pragma Ada_05;


with Interfaces;

package RTI.Osapi_Thread is
   pragma Pure;

   use type Interfaces.Unsigned_32;
   use type Interfaces.Integer_32;

   OPTION_DEFAULT             : constant Interfaces.Unsigned_32 := 16#00#; -- 0x00
   OPTION_FLOATING_POINT      : constant Interfaces.Unsigned_32 := 16#01#; -- 0x01
   OPTION_STDIO               : constant Interfaces.Unsigned_32 := 16#02#; -- 0x02
   OPTION_REALTIME_PRIORITY   : constant Interfaces.Unsigned_32 := 16#08#; -- 0x08
   OPTION_PRIORITY_ENFORCE    : constant Interfaces.Unsigned_32 := 16#10#; -- 0x10
   OPTION_CANCEL_ASYNCHRONOUS : constant Interfaces.Unsigned_32 := 16#20#; -- 0x20

   --
   --  Windows Specific
   --
   PRIORITY_HIGH         : constant Interfaces.Integer_32 := 3;
   PRIORITY_ABOVE_NORMAL : constant Interfaces.Integer_32 := 2;
   PRIORITY_NORMAL       : constant Interfaces.Integer_32 := 0;
   PRIORITY_BELOW_NORMAL : constant Interfaces.Integer_32 := -2;
   PRIORITY_LOW          : constant Interfaces.Integer_32 := -3;

   PRIORITY_DEFAULT : constant Interfaces.Integer_32 := PRIORITY_NORMAL;
   STACK_SIZE_DEFAULT : constant  Interfaces.Integer_32 := 0;

end RTI.Osapi_Thread;
