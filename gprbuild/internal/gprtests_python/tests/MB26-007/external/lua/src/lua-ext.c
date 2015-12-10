#include "lua.h"
#include "lauxlib.h"

/*This library is here for the sole purpose of the ADA bindings*/

/*this is needed, otherwise (especially on windows) the lua_tolstring
  function os considered to be a DLL import.*/
#define LUALIB

const char *
luext_version (void)
{
  return LUA_VERSION;
}

const char *
luext_release (void)
{
  return LUA_RELEASE;
}

LUA_INTEGER
luext_version_num (void)
{
  return LUA_VERSION_NUM;
}

const char *
luext_tostring (lua_State *ls, int index, size_t *len)
{
  size_t x;
  return lua_tolstring (ls, index, &x);
}

LUA_INTEGER luext_registry_index = LUA_REGISTRYINDEX;
