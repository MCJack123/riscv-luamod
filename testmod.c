#include "lua.h"
#include "lauxlib.h"
#include <string.h>
#include <errno.h>

static int test_foo(lua_State *L) {
    errno = lua_tointeger(L, 1);
    lua_pushstring(L, strerror(errno));
    return 1;
}

static luaL_Reg lib[] = {
    {"foo", test_foo},
    {NULL, NULL}
};

int luaopen_test(lua_State *L) {
    luaL_newlib(L, lib);
    return 1;
}
