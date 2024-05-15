#include "lua.h"
#include "lauxlib.h"

static int test_foo(lua_State *L) {
    lua_pushinteger(L, 4);
    lua_pushstring(L, "Hello World!");
    return 2;
}

static luaL_Reg lib[] = {
    {"foo", test_foo},
    {NULL, NULL}
};

int luaopen_test(lua_State *L) {
    luaL_newlib(L, lib);
    return 1;
}
