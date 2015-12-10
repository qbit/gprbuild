package Lua.Socket is

   procedure Open_LuaSocket( State : Lua.State_t );
   pragma Import( C, Open_LuaSocket, "luaopen_socket_core" );

end Lua.Socket;
