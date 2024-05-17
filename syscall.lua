local syscalls = {}

function syscalls:openat(dirfd, _path, flags)
    return -1
end

function syscalls:close(fd)
    return 0
end

function syscalls:mmap2(addr, length, prot, flags, fd, pgoffset)
    if not self.sysdata.mmap then
        self.sysdata.mmap = {base = self.endAddr, mapped = {}}
        for a = 0, self.endAddr, 4096 do self.sysdata.mmap.mapped[a / 4096] = true end
    end
    if addr == 0 then addr = self.sysdata.mmap.base end
    addr = math.ceil(addr / 4096) * 4096
    while self.sysdata.mmap.mapped[addr / 4096] do addr = addr + 4096 end
    for a = addr, addr + length - 1, 4096 do self.sysdata.mmap.mapped[a / 4096] = true end
    -- TODO: fd
    return addr
end

function syscalls:getrandom(_buf, len, flags)
    for i = 0, len - 1 do
        self.mem[_buf + i] = math.random(0, 255)
    end
    return len
end

local lua52 = require "lua52"
function syscalls:lua52(opt, ...)
    if not lua52[opt] then error("Unimplemented Lua call " .. opt) end
    --for k, v in pairs(lua52) do if k ~= opt and v == lua52[opt] then print(k) end end
    return lua52[opt](self, ...)
end

local syscall_list = {
    [56] = "openat",
    [57] = "close",
    [222] = "mmap2",
    [278] = "getrandom",
    [0x1b52] = "lua52"
}
for k, v in pairs(syscall_list) do syscalls[k] = syscalls[v] end

return syscalls
