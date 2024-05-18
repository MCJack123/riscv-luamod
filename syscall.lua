local syscalls = {}

function syscalls:openat(dirfd, _path, flags)
    return -1
end

function syscalls:close(fd)
    return 0
end

function syscalls:brk(brk)
    if not self.sysdata.brk then self.sysdata.brk = self.endAddr end
    local old = self.sysdata.brk
    self.sysdata.brk = math.min(brk, 0x1FF0000)
    return old
end

function syscalls:mmap2(addr, length, prot, flags, fd, pgoffset)
    if not self.sysdata.mmap then
        self.sysdata.mmap = {base = self.endAddr, mapped = {[0x1FFE] = true, [0x1FFF] = true, [0x2000] = true}}
        for a = 0, self.endAddr, 4096 do self.sysdata.mmap.mapped[a / 4096] = true end
    end
    if addr == 0 then addr = self.sysdata.mmap.base end
    addr = math.ceil(addr / 4096) * 4096
    while self.sysdata.mmap.mapped[addr / 4096] do addr = addr + 4096 end
    for a = addr, addr + length - 1, 4096 do self.sysdata.mmap.mapped[a / 4096] = true end
    -- TODO: fd
    return addr
end

function syscalls:prlimit64(pid, resource, _new_rlim, _old_rlim)
    if resource == 1 then return fs.getCapacity()
    elseif resource == 6 then return 1
    elseif resource == 7 then return 128
    elseif resource == 9 then return 0x2000000
    elseif resource == 13 then return 39
    else return 0x7FFFFFFF end
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
    [214] = "brk",
    [222] = "mmap2",
    [261] = "prlimit64",
    [278] = "getrandom",
    [0x1b52] = "lua52"
}
for k, v in pairs(syscall_list) do syscalls[k] = syscalls[v] end

return syscalls
