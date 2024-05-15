local syscalls = {}

function syscalls:open(_path, flags)

end

local lua52 = require "lua52"
function syscalls:lua52(opt, ...)
    if not lua52[opt] then error("Unimplemented Lua call " .. opt) end
    --for k, v in pairs(lua52) do if k ~= opt and v == lua52[opt] then print(k) end end
    return lua52[opt](self, ...)
end

local syscall_list = {
    [0x1b52] = "lua52"
}
for k, v in pairs(syscall_list) do syscalls[k] = syscalls[v] end

return syscalls
