-- $Id

generic
   -- the type we want to share with lua
   type User_Data_t is private;
package Lua.Light_User_Data is

   type Light_User_Data_Access_T is access all User_Data_t;

   -- we push a value
   -- this will allocate a new object
   procedure Push (State : Lua.State_t; UD : User_Data_t);

   -- we directly push an access type
   procedure Push (State : Lua.State_t; UD : Light_User_Data_Access_t);

   -- free() all objects which have been allocated by the value version of Push
   procedure Cleanup( State : Lua.State_t );

   -- returns a pointer to an object
   function Get (State : Lua.State_t; Index : Integer := 1) return Light_User_Data_Access_T;
   pragma Inline (Get);

end Lua.Light_User_Data;
