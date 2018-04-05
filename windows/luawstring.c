#include <windows.h>

#include <stdlib.h>

#include <lua.h>
#include <lauxlib.h>

extern void lua_pushwstring(lua_State *L, const wchar_t *wstring)
{
    char *utf8;
    int nbyte; /* including null terminator */

    if (!wstring) {
        lua_pushnil(L);
        return;
    }
    if (!(nbyte = WideCharToMultiByte(CP_UTF8, 0, wstring, -1, 0, 0, 0, 0))) {
        luaL_error(L, "charset conversion error");
    }
    if (!(utf8 = calloc(nbyte, 1))) {
        luaL_error(L, "out of memory");
    }
    if (WideCharToMultiByte(CP_UTF8, 0, wstring, -1, utf8, nbyte, 0, 0) != nbyte) {
        free(utf8);
        luaL_error(L, "charset conversion error");
    }
    lua_pushlstring(L, utf8, nbyte-1);
    free(utf8);
}

extern wchar_t *luaL_checkwstring(lua_State *L, int narg)
{
    const char *utf8;
    wchar_t *wstring;
    int nchar; /* including null terminator */

    utf8 = luaL_checkstring(L, narg);
    if (!(nchar = MultiByteToWideChar(CP_UTF8, 0, utf8, -1, 0, 0))) {
        luaL_error(L, "charset conversion error");
    }
    if (!(wstring = calloc(nchar, sizeof(*wstring)))) {
        luaL_error(L, "out of memory");
    }
    if (MultiByteToWideChar(CP_UTF8, 0, utf8, -1, wstring, nchar) != nchar) {
        free(wstring);
        luaL_error(L, "charset conversion error");
    }
    return wstring;
}
