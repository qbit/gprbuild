-- Lua standard libraries

with Lua;

package Lua.Lib is

   type Register_t is record
      Name : UB_Strings.Unbounded_String;
      Func : Lua.User_Function_t;
   end record;
   type Register_Access_t is access all Register_t;
   type Register_Array_t is array (Positive range <>) of Register_t;
   Empty_Register_Array_T : Register_Array_T(1..0) := ( others => ( UB_Strings.Null_Unbounded_String, null ) );


   function New_Metatable (State : Lua.State_t; Name : String) return Boolean;

   -- Opens ( creating if necessary ) a table containing a bunch of functions
   -- ( if Name /= """ ).
   -- The function return the library on the stack, so if you don't want to
   -- use it straightaway and just want to register functions, you mustn't
   -- forget to pop it.
   procedure Open_Library
     (State     : Lua.State_t;
      Name      : String;
      Functions : Register_Array_t;
      num_upValues  : Integer);

   -- opens the builtin standard libraries
   procedure Open_Base (State : Lua.State_t);
   pragma Import (C, Open_Base, "luaopen_base");

   procedure Open_Table (State : Lua.State_t);
   pragma Import (C, Open_Table, "luaopen_table");

   procedure Open_String (State : Lua.State_t);
   pragma Import (C, Open_String, "luaopen_string");

   procedure Open_IO (State : Lua.State_t);
   pragma Import (C, Open_IO, "luaopen_io");

   procedure Open_Math (State : Lua.State_t);
   pragma Import (C, Open_Math, "luaopen_math");

   procedure Open_Debug (State : Lua.State_t);
   pragma Import (C, Open_Debug, "luaopen_debug");

   -- all of them at the same time
   procedure Open_Libs (State : Lua.State_t);
   pragma Import (C, Open_Libs, "luaL_openlibs");

end Lua.Lib;
