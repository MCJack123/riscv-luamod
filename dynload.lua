local readelf = require "elf"

local function symbol(modules, baseAddress, sym)
    if sym.shndx == 0 or sym.value == 0 then -- UNDEF
        local name = sym.name
        --print(name)
        for _, m in pairs(modules) do
            for k, v in pairs(m.symbols) do
                if k == name or k:match("^([^@]+)@@") == name or k:gsub("@", "@@") == name then
                    --print(k)
                    return v
                end
            end
        end
        if sym.binding == "WEAK" then return 0 end
        error("unresolved symbol '" .. name .. "'")
    else
        return baseAddress + sym.value
    end
end

local function dynload(self, modules, name, baseAddress)
    baseAddress = baseAddress or 0
    local file = assert(io.open(shell.resolve and shell.resolve(name) or name, "rb"))
    local data = file:read("*a")
    file:close()
    local elf = readelf(data)
    if elf.machine ~= 0xF3 then error("Incompatible binary") end
    if not elf.dynamic then error("No dynamic section") end
    -- 1. Copy segments to memory
    local endAddr = baseAddress
    for _, v in ipairs(elf.segments) do
        if v.type == "LOAD" then
            ffi.copy(self.mem + baseAddress + v.address, v.data)
            endAddr = math.max(endAddr, baseAddress + v.address + #v.data)
        end
    end
    -- 2. Load dependencies
    for _, v in ipairs(elf.dynamic.needed) do
        endAddr = dynload(self, modules, v, math.ceil(endAddr / 0x1000) * 0x1000)
    end
    -- 3. Process relocations
    for _, v in pairs(elf.sections) do
        if v.relocations then
            for _, r in ipairs(v.relocations) do
                local addr = baseAddress + r.offset
                --print(name, r.type, r.offset, r.addend, r.symbol and r.symbol.name)
                if r.type == 1 then -- R_RISCV_32
                    self.mem32[addr / 4] = symbol(modules, baseAddress, r.symbol) + r.addend
                elseif r.type == 3 then -- R_RISCV_RELATIVE
                    self.mem32[addr / 4] = baseAddress + r.addend
                elseif r.type == 5 then -- R_RISCV_JUMP_SLOT
                    self.mem32[addr / 4] = symbol(modules, baseAddress, r.symbol)
                    --print(("%08x %08x"):format(addr, self.mem32[addr / 4]))
                end
            end
        end
    end
    -- 4. Run array initializers

    -- 5. Create exported symbol table and return
    local symbols = {}
    for _, v in ipairs(elf.sections[".dynsym"].symbols) do
        if v.binding ~= "LOCAL" and v.shndx ~= 0 and v.value ~= 0 then
            symbols[v.name] = baseAddress + v.value
        end
    end
    modules[name] = {
        baseAddress = baseAddress,
        size = endAddr - baseAddress,
        elf = elf,
        symbols = symbols
    }
    return endAddr
end

return dynload
