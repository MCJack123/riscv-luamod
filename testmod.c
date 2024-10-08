#include "lua.h"
#include "lauxlib.h"
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <locale.h>
#include <dirent.h>

static int test_foo(lua_State *L) {
    errno = lua_tointeger(L, 1);
    lua_pushstring(L, strerror(errno));
    return 1;
}

static int test_str(lua_State *L) {
    size_t sz;
    const char* str = lua_tolstring(L, 1, &sz);
    lua_pushunsigned(L, sz);
    lua_pushinteger(L, *str);
    return 2;
}

static int test_isdigit(lua_State *L) {
    const unsigned short* ct = *__ctype_b_loc();
    lua_pushinteger(L, __ctype_b_loc());
    lua_pushinteger(L, ct);
    lua_pushboolean(L, isdigit('1'));
    //for (int i = 0; i < 256; i++) lua_pushinteger(L, ct[i]);
    return 3;
}

static int test_list(lua_State *L) {
    DIR * dir = opendir("/riscv-luamod");
    if (dir) {
        lua_newtable(L);
        struct dirent * d;
        for (int i = 1; (d = readdir(dir)) != NULL; i++) {
            lua_pushstring(L, d->d_name);
            lua_rawseti(L, -2, i);
        }
        closedir(dir);
    } else {
        lua_pushnil(L);
    }
    return 1;
}

static luaL_Reg lib[] = {
    {"foo", test_foo},
    {"str", test_str},
    {"isdigit", test_isdigit},
    {"list", test_list},
    {NULL, NULL}
};

int luaopen_test(lua_State *L) {
    luaL_newlib(L, lib);
    return 1;
}
