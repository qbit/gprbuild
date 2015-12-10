with Ada.Exceptions;
package Lua.AuxLib is

   Lua_Exception : exception;
   Top_Of_Stack : constant Integer := -1;

   -- number of arguments
   procedure Check_N_Arguments (L : Lua.State_t; N_Arguments : Integer);

   -- generic checking function
   generic
      Lua_Type : Lua.Type_t;
      with function Is_A (L : State_t; Index : Integer) return Boolean;

   package Check_Argument_Type is
      -- checks the argument type of the stack at the given index
      procedure Check (L : Lua.State_t; Index : Integer);
   end Check_Argument_Type;

   -- type of arguments
   procedure Check_Number (L : Lua.State_t; Index : Integer);
   procedure Check_Boolean (L : Lua.State_t; Index : Integer);
   procedure Check_String (L : Lua.State_t; Index : Integer);
   procedure Check_Table (L : Lua.State_t; Index : Integer);

   -- Dump full stack
   procedure Dump_Stack (L : Lua.State_t);

   -- Retrieve Table value

   -- Assumes that the top element of the stack contains an array ( ie a
   -- table table { a,b,c,d} indexed by numbers )
   -- Please note the I parameter is *not* relative to the stack, but to the
   -- array, so if we have
   -- T = { 6,4,5,2 }
   -- then
   -- Get_Array_Value( L, 3 ) returns 5.
   --
   -- the procedure version leaves the result on the stack
   procedure Get_Array_Value (L : Lua.State_t; I : Integer);
   -- the function version actually returns a number
   function Get_Array_Value (L : Lua.State_t; I : Integer) return Number_t;

   -- Retrieve table field. Assumes that the top of the stack
   -- contains a table
   procedure Get_Table_Field (L : Lua.State_t; Field : String);

   -- exception wrapped command execution
   procedure Exec_File (L : Lua.State_t; Filename : String);
   procedure Exec_String (L : Lua.State_t; Statement : String; Buffer_Name : in String);

   procedure Protected_Call (L : State_t; Num_Arguments : Integer; Num_Results : Integer);

end Lua.AuxLib;
