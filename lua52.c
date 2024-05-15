#define _DEFAULT_SOURCE
#include <sys/syscall.h>
#include <unistd.h>
#include "lua.h"

#define LUA_SYSCALL 0x1b52

enum lua_call_id {
    LUA_SYSCALL_newstate,
    LUA_SYSCALL_close,
    LUA_SYSCALL_newthread,

    LUA_SYSCALL_atpanic,

    LUA_SYSCALL_version,

    LUA_SYSCALL_absindex,
    LUA_SYSCALL_gettop,
    LUA_SYSCALL_settop,
    LUA_SYSCALL_pushvalue,
    LUA_SYSCALL_remove,
    LUA_SYSCALL_insert,
    LUA_SYSCALL_replace,
    LUA_SYSCALL_copy,
    LUA_SYSCALL_checkstack,

    LUA_SYSCALL_xmove,

    LUA_SYSCALL_isnumber,
    LUA_SYSCALL_isstring,
    LUA_SYSCALL_iscfunction,
    LUA_SYSCALL_isuserdata,
    LUA_SYSCALL_type,
    LUA_SYSCALL_typename,

    LUA_SYSCALL_tonumberx,
    LUA_SYSCALL_tointegerx,
    LUA_SYSCALL_tounsignedx,
    LUA_SYSCALL_toboolean,
    LUA_SYSCALL_tolstring,
    LUA_SYSCALL_rawlen,
    LUA_SYSCALL_tocfunction,
    LUA_SYSCALL_touserdata,
    LUA_SYSCALL_tothread,
    LUA_SYSCALL_topointer,

    LUA_SYSCALL_arith,

    LUA_SYSCALL_rawequal,
    LUA_SYSCALL_compare,

    LUA_SYSCALL_pushnil,
    LUA_SYSCALL_pushnumber,
    LUA_SYSCALL_pushinteger,
    LUA_SYSCALL_pushunsigned,
    LUA_SYSCALL_pushlstring,
    LUA_SYSCALL_pushstring,
    LUA_SYSCALL_pushvfstring,
    LUA_SYSCALL_pushfstring,
    LUA_SYSCALL_pushcclosure,
    LUA_SYSCALL_pushboolean,
    LUA_SYSCALL_pushlightuserdata,
    LUA_SYSCALL_pushthread,

    LUA_SYSCALL_getglobal,
    LUA_SYSCALL_gettable,
    LUA_SYSCALL_getfield,
    LUA_SYSCALL_rawget,
    LUA_SYSCALL_rawgeti,
    LUA_SYSCALL_rawgetp,
    LUA_SYSCALL_createtable,
    LUA_SYSCALL_newuserdata,
    LUA_SYSCALL_getmetatable,
    LUA_SYSCALL_getuservalue,

    LUA_SYSCALL_setglobal,
    LUA_SYSCALL_settable,
    LUA_SYSCALL_setfield,
    LUA_SYSCALL_rawset,
    LUA_SYSCALL_rawseti,
    LUA_SYSCALL_rawsetp,
    LUA_SYSCALL_setmetatable,
    LUA_SYSCALL_setuservalue,

    LUA_SYSCALL_callk,
    LUA_SYSCALL_getctx,
    LUA_SYSCALL_pcallk,
    LUA_SYSCALL_load,
    LUA_SYSCALL_dump,

    LUA_SYSCALL_yieldk,
    LUA_SYSCALL_resume,
    LUA_SYSCALL_status,

    LUA_SYSCALL_gc,

    LUA_SYSCALL_error,
    LUA_SYSCALL_next,
    LUA_SYSCALL_concat,
    LUA_SYSCALL_len,
    LUA_SYSCALL_getallocf,
    LUA_SYSCALL_setallocf,

    LUA_SYSCALL_getstack,
    LUA_SYSCALL_getinfo,
    LUA_SYSCALL_getlocal,
    LUA_SYSCALL_setlocal,
    LUA_SYSCALL_getupvalue,
    LUA_SYSCALL_setupvalue,
    LUA_SYSCALL_upvalueid,
    LUA_SYSCALL_upvaluejoin,

    LUA_SYSCALL_sethook,
    LUA_SYSCALL_gethook,
    LUA_SYSCALL_gethookmask,
    LUA_SYSCALL_gethookcount
};

#define LUA_FUNC_V0(name) \
LUA_API void lua_##name (lua_State *L) {\
    syscall(LUA_SYSCALL, LUA_SYSCALL_##name, L);\
}

#define LUA_FUNC_V1(name, t1) \
LUA_API void lua_##name (lua_State *L, t1 a1) {\
    syscall(LUA_SYSCALL, LUA_SYSCALL_##name, L, a1);\
}

#define LUA_FUNC_V2(name, t1, t2) \
LUA_API void lua_##name (lua_State *L, t1 a1, t2 a2) {\
    syscall(LUA_SYSCALL, LUA_SYSCALL_##name, L, a1, a2);\
}

#define LUA_FUNC_R0(rt, name) \
LUA_API rt lua_##name (lua_State *L) {\
    return (rt)syscall(LUA_SYSCALL, LUA_SYSCALL_##name, L);\
}

#define LUA_FUNC_R1(rt, name, t1) \
LUA_API rt lua_##name (lua_State *L, t1 a1) {\
    return (rt)syscall(LUA_SYSCALL, LUA_SYSCALL_##name, L, a1);\
}

#define LUA_FUNC_R2(rt, name, t1, t2) \
LUA_API rt lua_##name (lua_State *L, t1 a1, t2 a2) {\
    return (rt)syscall(LUA_SYSCALL, LUA_SYSCALL_##name, L, a1, a2);\
}

static lua_Number version = 502;
const char lua_ident[] = "$Id: lua.c,v 1.285.1.4 2015/02/21 14:04:50 roberto Exp $";

LUA_API lua_State *lua_newstate(lua_Alloc f, void* ud) {
    return (lua_State*)syscall(LUA_SYSCALL, LUA_SYSCALL_newstate, f, ud);
}

LUA_FUNC_V0(close)
LUA_FUNC_R0(lua_State*, newthread)
LUA_FUNC_R1(lua_CFunction, atpanic, lua_CFunction)

LUA_API const lua_Number *lua_version(lua_State *L) {
    return &version;
}

LUA_FUNC_R1(int, absindex, int)
LUA_FUNC_R0(int, gettop)
LUA_FUNC_V1(settop, int)
LUA_FUNC_V1(pushvalue, int)
LUA_FUNC_V1(remove, int)
LUA_FUNC_V1(insert, int)
LUA_FUNC_V1(replace, int)
LUA_FUNC_V2(copy, int, int)
LUA_FUNC_R1(int, checkstack, int)
LUA_FUNC_V2(xmove, lua_State*, int)

LUA_FUNC_R1(int, isnumber, int)
LUA_FUNC_R1(int, isstring, int)
LUA_FUNC_R1(int, iscfunction, int)
LUA_FUNC_R1(int, isuserdata, int)
LUA_FUNC_R1(int, type, int)

static const char *const luaT_typenames_[] = {
  "no value",
  "nil", "boolean", "userdata", "number",
  "string", "table", "function", "userdata", "thread",
  "proto", "upval"  /* these last two cases are used for tests only */
};

LUA_API const char *lua_typename(lua_State *L, int tp) {
    return luaT_typenames_[tp+1];
}

LUA_FUNC_R2(lua_Number, tonumberx, int, int*)
LUA_FUNC_R2(lua_Integer, tointegerx, int, int*)
LUA_FUNC_R2(lua_Unsigned, tounsignedx, int, int*)
LUA_FUNC_R1(int, toboolean, int)

LUA_API const char *lua_tolstring(lua_State *L, int idx, size_t *len) {
    // TODO
    return NULL;
}

LUA_FUNC_R1(size_t, rawlen, int)
LUA_FUNC_R1(lua_CFunction, tocfunction, int)
LUA_FUNC_R1(void*, touserdata, int)
LUA_FUNC_R1(lua_State*, tothread, int)
LUA_FUNC_R1(const void*, topointer, int)

LUA_FUNC_V1(arith, int)
LUA_FUNC_R2(int, rawequal, int, int)

LUA_API int lua_compare (lua_State *L, int idx1, int idx2, int op) {
    return (int)syscall(LUA_SYSCALL, LUA_SYSCALL_compare, L, idx1, idx2, op);
}

LUA_FUNC_V0(pushnil)
LUA_FUNC_V1(pushnumber, lua_Number)
LUA_FUNC_V1(pushinteger, lua_Integer)
LUA_FUNC_V1(pushunsigned, lua_Unsigned)
LUA_FUNC_R2(const char*, pushlstring, const char*, size_t)
LUA_FUNC_R1(const char*, pushstring, const char*)

LUA_API const char *lua_pushvfstring(lua_State *L, const char *fmt, va_list argp) {
    // TODO
    syscall(LUA_SYSCALL, LUA_SYSCALL_pushstring, L, fmt);
    return NULL;
}

LUA_API const char *lua_pushfstring(lua_State *L, const char *fmt, ...) {
    // TODO
    return NULL;
}

LUA_FUNC_V2(pushcclosure, lua_CFunction, int)
LUA_FUNC_V1(pushboolean, int)
LUA_FUNC_V1(pushlightuserdata, void*)
LUA_FUNC_R0(int, pushthread)

LUA_FUNC_V1(getglobal, const char*)
LUA_FUNC_V1(gettable, int)
LUA_FUNC_V2(getfield, int, const char*)
LUA_FUNC_V1(rawget, int)
LUA_FUNC_V2(rawgeti, int, int)
LUA_FUNC_V2(rawgetp, int, const void*)
LUA_FUNC_V2(createtable, int, int)
LUA_FUNC_R1(void*, newuserdata, size_t)
LUA_FUNC_R1(int, getmetatable, int)
LUA_FUNC_V1(getuservalue, int)

LUA_FUNC_V1(setglobal, const char*)
LUA_FUNC_V1(settable, int)
LUA_FUNC_V2(setfield, int, const char*)
LUA_FUNC_V1(rawset, int)
LUA_FUNC_V2(rawseti, int, int)
LUA_FUNC_V2(rawsetp, int, const void*)
LUA_FUNC_R1(int, setmetatable, int)
LUA_FUNC_V1(setuservalue, int)

LUA_API void lua_callk(lua_State *L, int nargs, int nresults, int ctx, lua_CFunction k) {
    syscall(LUA_SYSCALL, LUA_SYSCALL_callk, L, nargs, nresults, ctx, k);
}

LUA_FUNC_R1(int, getctx, int*)

LUA_API int lua_pcallk(lua_State *L, int nargs, int nresults, int errfunc, int ctx, lua_CFunction k) {
    return (int)syscall(LUA_SYSCALL, LUA_SYSCALL_pcallk, L, nargs, nresults, errfunc, ctx, k);
}

LUA_API int lua_load(lua_State *L, lua_Reader reader, void *dt, const char *chunkname, const char *mode) {
    return (int)syscall(LUA_SYSCALL, LUA_SYSCALL_load, L, reader, dt, chunkname, mode);
}

LUA_FUNC_R2(int, dump, lua_Writer, void*)

LUA_API int lua_yieldk(lua_State *L, int nresults, int ctx, lua_CFunction k) {
    return (int)syscall(LUA_SYSCALL, LUA_SYSCALL_yieldk, L, nresults, ctx, k);
}

LUA_FUNC_R2(int, resume, lua_State*, int)
LUA_FUNC_R0(int, status)

LUA_FUNC_R2(int, gc, int, int)

LUA_FUNC_R0(int, error)
LUA_FUNC_R1(int, next, int)
LUA_FUNC_V1(concat, int)
LUA_FUNC_V1(len, int)
LUA_FUNC_R1(lua_Alloc, getallocf, void**)
LUA_FUNC_V2(setallocf, lua_Alloc, void*)

LUA_FUNC_R2(int, getstack, int, lua_Debug*)
LUA_FUNC_R2(int, getinfo, const char*, lua_Debug*)
LUA_FUNC_R2(const char*, getlocal, const lua_Debug*, int)
LUA_FUNC_R2(const char*, setlocal, const lua_Debug*, int)
LUA_FUNC_R2(const char*, getupvalue, int, int)
LUA_FUNC_R2(const char*, setupvalue, int, int)
LUA_FUNC_R2(void*, upvalueid, int, int)

LUA_API void lua_upvaluejoin(lua_State *L, int fidx1, int n1, int fidx2, int n2) {
    syscall(LUA_SYSCALL, LUA_SYSCALL_upvaluejoin, L, fidx1, n1, fidx2, n2);
}

LUA_API int lua_sethook(lua_State *L, lua_Hook func, int mask, int count) {
    return (int)syscall(LUA_SYSCALL, LUA_SYSCALL_sethook, L, func, mask, count);
}

LUA_FUNC_R0(lua_Hook, gethook)
LUA_FUNC_R0(int, gethookmask)
LUA_FUNC_R0(int, gethookcount)
