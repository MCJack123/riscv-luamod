-- Licensed under GPLv2
local RISCV = {reg = {}, pc = 0, syscalls = require "syscall", opcodes = {[0x63] = {}, [0x03] = {}, [0x23] = {}, [0x13] = {}, [0x33] = {}, [0x2F] = {[2] = true}}, mult_opcodes = {}, atomic_opcodes = {}, atomic_rs = {}, halt = false, sysdata = {}}
for i = 1, 31 do RISCV.reg[i] = 0 end
setmetatable(RISCV.reg, {__index = function() return 0 end, __newindex = function() end})
if ffi then
    print("Using FFI acceleration")
    RISCV.mem = ffi.new("uint8_t[?]", 0x2010000)
    RISCV.mem16 = ffi.cast("uint16_t*", RISCV.mem)
    RISCV.mem32 = ffi.cast("uint32_t*", RISCV.mem)
    RISCV.fficopy, RISCV.ffistring = ffi.copy, ffi.string
else
    local function __add(self, offset)
        return setmetatable({}, {
            __index = function(_, idx) return self[offset+idx] end,
            __newindex = function(_, idx, val) self[offset+idx] = val end,
            __add = __add
        })
    end
    RISCV.mem = setmetatable({}, {
        __index = function() return 0 end,
        __add = __add
    })
    RISCV.mem16 = setmetatable({}, {
        __index = function(_, idx) return RISCV.mem[idx*2] + RISCV.mem[idx*2+1] * 256 end,
        __newindex = function(_, idx, val)
            RISCV.mem[idx*2] = bit32.extract(val, 0, 8)
            RISCV.mem[idx*2+1] = bit32.extract(val, 8, 8)
        end,
        __add = __add
    })
    RISCV.mem32 = setmetatable({}, {
        __index = function(_, idx) return RISCV.mem[idx*4] + RISCV.mem[idx*4+1] * 256 + RISCV.mem[idx*4+2] * 65536 + RISCV.mem[idx*4+3] * 16777216 end,
        __newindex = function(_, idx, val)
            RISCV.mem[idx*4] = bit32.extract(val, 0, 8)
            RISCV.mem[idx*4+1] = bit32.extract(val, 8, 8)
            RISCV.mem[idx*4+2] = bit32.extract(val, 16, 8)
            RISCV.mem[idx*4+3] = bit32.extract(val, 24, 8)
        end,
        __add = __add
    })
    function RISCV.fficopy(dest, src, size)
        if type(src) == "string" then
            for i, c in src:gmatch "()(.)" do
                dest[i-1] = c:byte()
            end
        else
            for i = 0, size - 1 do
                dest[i] = src[i]
            end
        end
    end
    function RISCV.ffistring(ptr, size)
        local retval = ""
        if size then
            for i = 0, size - 1 do retval = retval .. string.char(ptr[i]) end
        else
            for i = 0, math.huge do
                local c = ptr[i]
                if c == 0 then break end
                retval = retval .. string.char(c)
            end
        end
        return retval
    end
end

local function fiximm(bits) return function(inst)
    if bit32.btest(inst.imm, 2^(bits-1)) then inst.simm = bit32.bor(inst.imm, bit32.bnot(2^bits-1)) - 0x100000000
    else inst.simm = inst.imm end
    return inst
end end

local function decodeR(inst)
    return {
        inst = inst,
        opcode = bit32.extract(inst, 0, 7),
        rd = bit32.extract(inst, 7, 5),
        funct3 = bit32.extract(inst, 12, 3),
        rs1 = bit32.extract(inst, 15, 5),
        rs2 = bit32.extract(inst, 20, 5),
        funct7 = bit32.extract(inst, 25, 7)
    }
end

local function decodeI(inst)
    return fiximm(12) {
        inst = inst,
        opcode = bit32.extract(inst, 0, 7),
        rd = bit32.extract(inst, 7, 5),
        funct3 = bit32.extract(inst, 12, 3),
        rs1 = bit32.extract(inst, 15, 5),
        imm = bit32.extract(inst, 20, 12)
    }
end

local function decodeS(inst)
    return fiximm(12) {
        inst = inst,
        opcode = bit32.extract(inst, 0, 7),
        funct3 = bit32.extract(inst, 12, 3),
        rs1 = bit32.extract(inst, 15, 5),
        rs2 = bit32.extract(inst, 20, 5),
        imm = bit32.bor(bit32.extract(inst, 7, 5), bit32.lshift(bit32.extract(inst, 25, 7), 5))
    }
end

local function decodeB(inst)
    return fiximm(13) {
        inst = inst,
        opcode = bit32.extract(inst, 0, 7),
        funct3 = bit32.extract(inst, 12, 3),
        rs1 = bit32.extract(inst, 15, 5),
        rs2 = bit32.extract(inst, 20, 5),
        imm = bit32.bor(
            bit32.lshift(bit32.extract(inst, 7, 1), 11),
            bit32.lshift(bit32.extract(inst, 8, 4), 1),
            bit32.lshift(bit32.extract(inst, 25, 6), 5),
            bit32.lshift(bit32.extract(inst, 31, 1), 12)
        )
    }
end

local function decodeU(inst)
    return fiximm(32) {
        inst = inst,
        opcode = bit32.extract(inst, 0, 7),
        rd = bit32.extract(inst, 7, 5),
        imm = bit32.band(inst, 0xFFFFF000)
    }
end

local function decodeJ(inst)
    return fiximm(21) {
        inst = inst,
        opcode = bit32.extract(inst, 0, 7),
        rd = bit32.extract(inst, 7, 5),
        imm = bit32.bor(
            bit32.lshift(bit32.extract(inst, 12, 8), 12),
            bit32.lshift(bit32.extract(inst, 20, 1), 11),
            bit32.lshift(bit32.extract(inst, 21, 10), 1),
            bit32.lshift(bit32.extract(inst, 31, 1), 20)
        )
    }
end

local opcode_modes = {
    [0x37] = decodeU, -- LUI
    [0x17] = decodeU, -- AUIPC
    [0x6F] = decodeJ, -- JAL
    [0x67] = decodeI, -- JALR
    [0x63] = decodeB, -- B*
    [0x03] = decodeI, -- L*
    [0x23] = decodeS, -- S*
    [0x13] = decodeI, -- immediate arith
    [0x33] = decodeR, -- arith
    [0x0F] = decodeI, -- FENCE
    [0x73] = decodeI, -- E*
    [0x2F] = decodeR, -- atomic
}

RISCV.opcodes[0x37] = function(pc, inst) -- LUI
    return ([[
        -- %08X: LUI
        self.reg[%d] = %d
    ]]):format(pc, inst.rd, inst.imm)
end

RISCV.opcodes[0x17] = function(pc, inst) -- AUIPC
    return ([[
        -- %08X: AUIPC
        self.reg[%d] = %d
    ]]):format(pc, inst.rd, (pc - 4 + inst.imm) % 0x100000000)
end

RISCV.opcodes[0x6F] = function(pc, inst) -- JAL
    if (pc + inst.simm - 4) % 4 ~= 0 then error("unaligned jump to " .. (pc + inst.simm - 4)) end
    return ([[
        -- %08X: JAL
        self.reg[%d] = %d
        return self.traces[%d](self)
    ]]):format(pc, inst.rd, pc, pc + inst.simm - 4), true
end

RISCV.opcodes[0x67] = function(pc, inst) -- JALR
    return ([[
        -- %08X: JALR
        local pc = bit32.band(self.reg[%d] + %d, 0xFFFFFFFE)
        self.reg[%d] = %d
        return self.traces[pc](self)
    ]]):format(pc, inst.rs1, inst.simm, inst.rd, pc), true
end

RISCV.opcodes[0x63][0] = function(pc, inst) -- BEQ
    return ([[
        -- %08X: BEQ
        if self.reg[%d] == self.reg[%d] then return self.traces[%d](self)
        else return self.traces[%d](self) end
    ]]):format(pc, inst.rs1, inst.rs2, pc + inst.simm - 4, pc), true
end

RISCV.opcodes[0x63][1] = function(pc, inst) -- BNE
    return ([[
        -- %08X: BNE
        if self.reg[%d] ~= self.reg[%d] then return self.traces[%d](self)
        else return self.traces[%d](self) end
    ]]):format(pc, inst.rs1, inst.rs2, pc + inst.simm - 4, pc), true
end

RISCV.opcodes[0x63][4] = function(pc, inst) -- BLT
    return ([[
        -- %08X: BLT
        local ra, rb = self.reg[%d], self.reg[%d]
        if ra >= 0x80000000 then ra = ra - 0x100000000 end
        if rb >= 0x80000000 then rb = rb - 0x100000000 end
        if ra < rb then return self.traces[%d](self)
        else return self.traces[%d](self) end
    ]]):format(pc, inst.rs1, inst.rs2, pc + inst.simm - 4, pc), true
end

RISCV.opcodes[0x63][5] = function(pc, inst) -- BGE
    return ([[
        -- %08X: BGE
        local ra, rb = self.reg[%d], self.reg[%d]
        if ra >= 0x80000000 then ra = ra - 0x100000000 end
        if rb >= 0x80000000 then rb = rb - 0x100000000 end
        if ra >= rb then return self.traces[%d](self)
        else return self.traces[%d](self) end
    ]]):format(pc, inst.rs1, inst.rs2, pc + inst.simm - 4, pc), true
end

RISCV.opcodes[0x63][6] = function(pc, inst) -- BLTU
    return ([[
        -- %08X: BLTU
        if self.reg[%d] < self.reg[%d] then return self.traces[%d](self)
        else return self.traces[%d](self) end
    ]]):format(pc, inst.rs1, inst.rs2, pc + inst.simm - 4, pc), true
end

RISCV.opcodes[0x63][7] = function(pc, inst) -- BGEU
    return ([[
        -- %08X: BGEU
        if self.reg[%d] >= self.reg[%d] then return self.traces[%d](self)
        else return self.traces[%d](self) end
    ]]):format(pc, inst.rs1, inst.rs2, pc + inst.simm - 4, pc), true
end

RISCV.opcodes[0x03][0] = function(pc, inst) -- LB
    return ([[
        -- %08X: LB
        self.reg[%d] = self.mem[self.reg[%d] + %d]
        if self.reg[%d] >= 0x80 then self.reg[%d] = self.reg[%d] + 0xFFFFFF00 end
    ]]):format(pc, inst.rd, inst.rs1, inst.simm, inst.rd, inst.rd, inst.rd)
end

RISCV.opcodes[0x03][1] = function(pc, inst) -- LH
    return ([[
        -- %08X: LH
        local addr = self.reg[%d] + %d
        if addr %% 2 ~= 0 then self.reg[%d] = self.mem[addr] + self.mem[addr+1] * 256
        else self.reg[%d] = self.mem16[addr / 2] end
        if self.reg[%d] >= 0x8000 then self.reg[%d] = self.reg[%d] + 0xFFFF0000 end
    ]]):format(pc, inst.rs1, inst.simm, inst.rd, inst.rd, inst.rd, inst.rd, inst.rd)
end

RISCV.opcodes[0x03][2] = function(pc, inst) -- LW
    return ([[
        -- %08X: LW
        local addr = self.reg[%d] + %d
        if addr %% 4 ~= 0 then self.reg[%d] = self.mem[addr] + self.mem[addr+1] * 256 + self.mem[addr+2] * 65536 + self.mem[addr+3] * 16777216
        else self.reg[%d] = self.mem32[addr / 4] end
    ]]):format(pc, inst.rs1, inst.simm, inst.rd, inst.rd)
end

RISCV.opcodes[0x03][4] = function(pc, inst) -- LBU
    return ([[
        -- %08X: LBU
        self.reg[%d] = self.mem[self.reg[%d] + %d]
    ]]):format(pc, inst.rd, inst.rs1, inst.simm)
end

RISCV.opcodes[0x03][5] = function(pc, inst) -- LHU
    return ([[
        -- %08X: LHU
        local addr = self.reg[%d] + %d
        if addr %% 2 ~= 0 then self.reg[%d] = self.mem[addr] + self.mem[addr+1] * 256
        else self.reg[%d] = self.mem16[addr / 2] end
    ]]):format(pc, inst.rs1, inst.simm, inst.rd, inst.rd)
end

RISCV.opcodes[0x23][0] = function(pc, inst) -- SB
    return ([[
        -- %08X: SB
        self.mem[self.reg[%d] + %d] = self.reg[%d] %% 256
    ]]):format(pc, inst.rs1, inst.simm, inst.rs2)
end

RISCV.opcodes[0x23][1] = function(pc, inst) -- SH
    return ([[
        -- %08X: SH
        local addr = self.reg[%d] + %d
        if addr %% 2 ~= 0 then
            self.mem[addr] = bit32.extract(self.reg[%d], 0, 8)
            self.mem[addr+1] = bit32.extract(self.reg[%d], 8, 8)
        else self.mem16[addr / 2] = self.reg[%d] %% 65536 end
    ]]):format(pc, inst.rs1, inst.simm, inst.rs2, inst.rs2, inst.rs2)
end

RISCV.opcodes[0x23][2] = function(pc, inst) -- SW
    return ([[
        -- %08X: SW
        local addr = self.reg[%d] + %d
        if addr %% 4 ~= 0 then
            self.mem[addr] = bit32.extract(self.reg[%d], 0, 8)
            self.mem[addr+1] = bit32.extract(self.reg[%d], 8, 8)
            self.mem[addr+2] = bit32.extract(self.reg[%d], 16, 8)
            self.mem[addr+3] = bit32.extract(self.reg[%d], 24, 8)
        else self.mem32[addr / 4] = self.reg[%d] end
    ]]):format(pc, inst.rs1, inst.simm, inst.rs2, inst.rs2, inst.rs2, inst.rs2, inst.rs2)
end

RISCV.opcodes[0x13][0] = function(pc, inst) -- ADDI
    if inst.rs1 == 0 then
        return ([[
        -- %08X: LI
        self.reg[%d] = %d
        ]]):format(pc, inst.rd, inst.simm % 0x100000000)
    elseif inst.simm == 0 then
        return ([[
        -- %08X: MR
        self.reg[%d] = self.reg[%d]
        ]]):format(pc, inst.rd, inst.rs1)
    else
        return ([[
        -- %08X: ADDI
        self.reg[%d] = (self.reg[%d] + %d) %% 0x100000000
        ]]):format(pc, inst.rd, inst.rs1, inst.simm)
    end
end

RISCV.opcodes[0x13][2] = function(pc, inst) -- SLTI
    return ([[
        -- %08X: SLTI
        local rs = self.reg[%d]
        if rs >= 0x80000000 then rs = rs - 0x100000000 end
        self.reg[%d] = rs < %d and 1 or 0
    ]]):format(pc, inst.rs1, inst.rd, inst.simm)
end

RISCV.opcodes[0x13][3] = function(pc, inst) -- SLTIU
    local imm = inst.simm
    if imm < 0 then imm = imm + 0x100000000 end
    return ([[
        -- %08X: SLTIU
        self.reg[%d] = self.reg[%d] < %d and 1 or 0
    ]]):format(pc, inst.rd, inst.rs1, imm)
end

RISCV.opcodes[0x13][4] = function(pc, inst) -- XORI
    return ([[
        -- %08X: XORI
        self.reg[%d] = bit32.bxor(self.reg[%d], %d)
    ]]):format(pc, inst.rd, inst.rs1, inst.simm % 0x100000000)
end

RISCV.opcodes[0x13][6] = function(pc, inst) -- ORI
    return ([[
        -- %08X: ORI
        self.reg[%d] = bit32.bor(self.reg[%d], %d)
    ]]):format(pc, inst.rd, inst.rs1, inst.simm % 0x100000000)
end

RISCV.opcodes[0x13][7] = function(pc, inst) -- ANDI
    return ([[
        -- %08X: ANDI
        self.reg[%d] = bit32.band(self.reg[%d], %d)
    ]]):format(pc, inst.rd, inst.rs1, inst.simm % 0x100000000)
end

RISCV.opcodes[0x13][1] = function(pc, inst) -- SLLI
    return ([[
        -- %08X: SLLI
        self.reg[%d] = bit32.lshift(self.reg[%d], %d)
    ]]):format(pc, inst.rd, inst.rs1, bit32.band(inst.imm, 0x1F))
end

RISCV.opcodes[0x13][5] = function(pc, inst) -- SRLI/SRAI
    return ([[
        -- %08X: SRLI/SRAI
        self.reg[%d] = bit32.%srshift(self.reg[%d], %d)
    ]]):format(pc, inst.rd, bit32.btest(inst.imm, 0x400) and "a" or "", inst.rs1, bit32.band(inst.imm, 0x1F))
end

RISCV.opcodes[0x33][0] = function(pc, inst) -- ADD/SUB
    return ([[
        -- %08X: ADD/SUB
        self.reg[%d] = (self.reg[%d] %s self.reg[%d]) %% 0x100000000
    ]]):format(pc, inst.rd, inst.rs1, bit32.btest(inst.funct7, 0x20) and "-" or "+", inst.rs2)
end

RISCV.opcodes[0x33][1] = function(pc, inst) -- SLL
    return ([[
        -- %08X: SLL
        self.reg[%d] = bit32.lshift(self.reg[%d], bit32.band(self.reg[%d], 0x1F))
    ]]):format(pc, inst.rd, inst.rs1, inst.rs2)
end

RISCV.opcodes[0x33][2] = function(pc, inst) -- SLT
    return ([[
        -- %08X: SLT
        local ra, rb = self.reg[%d], self.reg[%d]
        if ra >= 0x80000000 then ra = ra - 0x100000000 end
        if rb >= 0x80000000 then rb = rb - 0x100000000 end
        self.reg[%d] = ra < rb and 1 or 0
    ]]):format(pc, inst.rs1, inst.rs2, inst.rd)
end

RISCV.opcodes[0x33][3] = function(pc, inst) -- SLTU
    return ([[
        -- %08X: SLTU
        self.reg[%d] = self.reg[%d] < self.reg[%d] and 1 or 0
    ]]):format(pc, inst.rd, inst.rs1, inst.rs2)
end

RISCV.opcodes[0x33][4] = function(pc, inst) -- XOR
    return ([[
        -- %08X: XOR
        self.reg[%d] = bit32.bxor(self.reg[%d], self.reg[%d])
    ]]):format(pc, inst.rd, inst.rs1, inst.rs2)
end

RISCV.opcodes[0x33][5] = function(pc, inst) -- SRL/SRA
    return ([[
        -- %08X: SRL/SRA
        self.reg[%d] = bit32.%srshift(self.reg[%d], bit32.band(self.reg[%d], 0x1F))
    ]]):format(pc, inst.rd, bit32.btest(inst.funct7, 0x20) and "a" or "", inst.rs1, inst.rs2)
end

RISCV.opcodes[0x33][6] = function(pc, inst) -- OR
    return ([[
        -- %08X: OR
        self.reg[%d] = bit32.bor(self.reg[%d], self.reg[%d])
    ]]):format(pc, inst.rd, inst.rs1, inst.rs2)
end

RISCV.opcodes[0x33][7] = function(pc, inst) -- AND
    return ([[
        -- %08X: AND
        self.reg[%d] = bit32.band(self.reg[%d], self.reg[%d])
    ]]):format(pc, inst.rd, inst.rs1, inst.rs2)
end

RISCV.opcodes[0x0F] = function(pc, inst) -- FENCE
    -- do nothing
    return ("-- %08X: FENCE\n"):format(pc)
end

RISCV.opcodes[0x73] = function(pc, inst) -- ECALL/EBREAK
    if inst.funct3 ~= 0 then return ("-- %08X: ECALL Zicsr\n"):format(pc) end -- Zicsr not implemented
    if inst.imm == 0 then return [=[
        if self.syscalls[self.reg[17]] then self.reg[10] = self.syscalls[self.reg[17]](self, table.unpack(self.reg, 10, 16)) end
        if self.halt then return end
    ]=]
    elseif inst.imm == 0x302 then return [=[
        return self.traces[self.reg[5]](self)
    ]=], true end
end

RISCV.mult_opcodes[0] = function(pc, inst) -- MUL
    return ([[
        -- %08X: MUL
        local ra, rb = self.reg[%d], self.reg[%d]
        if ra >= 0x80000000 then ra = ra - 0x100000000 end
        if rb >= 0x80000000 then rb = rb - 0x100000000 end
        self.reg[%d] = math.abs((ra * rb) %% 0x100000000)
    ]]):format(pc, inst.rs1, inst.rs2, inst.rd)
end

RISCV.mult_opcodes[3] = function(pc, inst) -- MULHU
    return ([[
        -- %08X: MULHU
        self.reg[%d] = math.floor((self.reg[%d] * self.reg[%d]) / 0x100000000)
    ]]):format(pc, inst.rd, inst.rs1, inst.rs2)
end

RISCV.mult_opcodes[2] = function(pc, inst) -- MULHSU
    return ([[
        -- %08X: MULHSU
        local ra = self.reg[%d]
        if ra >= 0x80000000 then ra = ra - 0x100000000 end
        local rd = math.floor((ra * self.reg[%d]) / 0x100000000)
        if rd < 0 then rd = rd + 0x100000000 end
        self.reg[%d] = rd
    ]]):format(pc, inst.rs1, inst.rs2, inst.rd)
end

RISCV.mult_opcodes[1] = function(pc, inst) -- MULH
    return ([[
        -- %08X: MULH
        local ra, rb = self.reg[%d], self.reg[%d]
        if ra >= 0x80000000 then ra = ra - 0x100000000 end
        if rb >= 0x80000000 then rb = rb - 0x100000000 end
        local rd = math.floor((ra * rb) / 0x100000000)
        if rd < 0 then rd = rd + 0x100000000 end
        self.reg[%d] = rd
    ]]):format(pc, inst.rs1, inst.rs2, inst.rd)
end

RISCV.mult_opcodes[4] = function(pc, inst) -- DIV
    return ([[
        -- %08X: DIV
        if self.reg[%d] == 0 then
            self.reg[%d] = 0xFFFFFFFF
        else
            local ra, rb = self.reg[%d], self.reg[%d]
            if ra >= 0x80000000 then ra = ra - 0x100000000 end
            if rb >= 0x80000000 then rb = rb - 0x100000000 end
            local res = ra / rb
            if res < 0 then self.reg[%d] = math.ceil(res) + 0x100000000
            else self.reg[%d] = math.floor(res) end
        end
    ]]):format(pc, inst.rs2, inst.rd, inst.rs1, inst.rs2, inst.rd, inst.rd)
end

RISCV.mult_opcodes[5] = function(pc, inst) -- DIVU
    return ([[
        -- %08X: DIVU
        if self.reg[%d] == 0 then self.reg[%d] = 0xFFFFFFFF
        else self.reg[%d] = math.floor(self.reg[%d] / self.reg[%d]) end
    ]]):format(pc, inst.rs2, inst.rd, inst.rd, inst.rs1, inst.rs2)
end

RISCV.mult_opcodes[6] = function(pc, inst) -- REM
    return ([[
        -- %08X: REM
        if self.reg[%d] == 0 then
            self.reg[%d] = self.reg[%d]
        else
            local ra, rb = self.reg[%d], self.reg[%d]
            if ra >= 0x80000000 then ra = ra - 0x100000000 end
            if rb >= 0x80000000 then rb = rb - 0x100000000 end
            local res = math.fmod(ra, rb)
            if res < 0 then self.reg[%d] = math.ceil(res) + 0x100000000
            else self.reg[%d] = math.floor(res) end
        end
    ]]):format(pc, inst.rs2, inst.rd, inst.rs1, inst.rs1, inst.rs2, inst.rd, inst.rd)
end

RISCV.mult_opcodes[7] = function(pc, inst) -- REMU
    return ([[
        -- %08X: REMU
        if self.reg[%d] == 0 then self.reg[%d] = self.reg[%d]
        else self.reg[%d] = self.reg[%d] %% self.reg[%d] end
    ]]):format(pc, inst.rs2, inst.rd, inst.rs1, inst.rd, inst.rs1, inst.rs2)
end

RISCV.atomic_opcodes[0] = function(pc, inst) -- AMOADD.W
    return ([[
        -- %08X: AMOADD.W
        local addr = self.reg[%d]
        if addr %% 4 ~= 0 then error("unaligned AMO instruction") end
        local val = self.mem32[addr / 4]
        self.reg[%d] = val
        self.mem32[addr / 4] = (val + self.reg[%d]) %% 0x100000000
    ]]):format(pc, inst.rs1, inst.rd, inst.rs2)
end

RISCV.atomic_opcodes[1] = function(pc, inst) -- AMOSWAP.W
    return ([[
        -- %08X: AMOSWAP.W
        local addr = self.reg[%d]
        if addr %% 4 ~= 0 then error("unaligned AMO instruction") end
        local val = self.mem32[addr / 4]
        self.reg[%d] = val
        self.mem32[addr / 4] = self.reg[%d]
    ]]):format(pc, inst.rs1, inst.rd, inst.rs2)
end

RISCV.atomic_opcodes[2] = function(pc, inst) -- LR.W
    return ([[
        -- %08X: LR.W
        local addr = self.reg[%d]
        if addr %% 4 ~= 0 then error("unaligned AMO instruction")
        else self.reg[%d] = self.mem32[addr / 4] end
        self.atomic_rs[addr / 4] = true
    ]]):format(pc, inst.rs1, inst.rd)
end

RISCV.atomic_opcodes[3] = function(pc, inst) -- SC.W
    return ([[
        -- %08X: SC.W
        local addr = self.reg[%d]
        if addr %% 4 ~= 0 then error("unaligned AMO instruction")
        elseif self.atomic_rs[addr / 4] then
            self.mem32[addr / 4] = self.reg[%d]
            self.reg[%d] = 0
        else
            self.reg[%d] = 1
        end
        self.atomic_rs[addr / 4] = nil
    ]]):format(pc, inst.rs1, inst.rs2, inst.rd, inst.rd)
end

RISCV.atomic_opcodes[4] = function(pc, inst) -- AMOXOR.W
    return ([[
        -- %08X: AMOXOR.W
        local addr = self.reg[%d]
        if addr %% 4 ~= 0 then error("unaligned AMO instruction") end
        local val = self.mem32[addr / 4]
        self.reg[%d] = val
        self.mem32[addr / 4] = bit32.bxor(val, self.reg[%d])
    ]]):format(pc, inst.rs1, inst.rd, inst.rs2)
end

RISCV.atomic_opcodes[8] = function(pc, inst) -- AMOOR.W
    return ([[
        -- %08X: AMOOR.W
        local addr = self.reg[%d]
        if addr %% 4 ~= 0 then error("unaligned AMO instruction") end
        local val = self.mem32[addr / 4]
        self.reg[%d] = val
        self.mem32[addr / 4] = bit32.bor(val, self.reg[%d])
    ]]):format(pc, inst.rs1, inst.rd, inst.rs2)
end

RISCV.atomic_opcodes[12] = function(pc, inst) -- AMOAND.W
    return ([[
        -- %08X: AMOAND.W
        local addr = self.reg[%d]
        if addr %% 4 ~= 0 then error("unaligned AMO instruction") end
        local val = self.mem32[addr / 4]
        self.reg[%d] = val
        self.mem32[addr / 4] = bit32.band(val, self.reg[%d])
    ]]):format(pc, inst.rs1, inst.rd, inst.rs2)
end

RISCV.atomic_opcodes[16] = function(pc, inst) -- AMOMIN.W
    return ([[
        -- %08X: AMOMIN.W
        local addr = self.reg[%d]
        if addr %% 4 ~= 0 then error("unaligned AMO instruction") end
        local val, val2 = self.mem32[addr / 4], self.reg[%d]
        local ra, rb = val, val2
        if ra >= 0x80000000 then ra = ra - 0x100000000 end
        if rb >= 0x80000000 then rb = rb - 0x100000000 end
        self.reg[%d] = val
        self.mem32[addr / 4] = ra < rb and val or val2
    ]]):format(pc, inst.rs1, inst.rs2, inst.rd)
end

RISCV.atomic_opcodes[20] = function(pc, inst) -- AMOMAX.W
    return ([[
        -- %08X: AMOMAX.W
        local addr = self.reg[%d]
        if addr %% 4 ~= 0 then error("unaligned AMO instruction") end
        local val, val2 = self.mem32[addr / 4], self.reg[%d]
        local ra, rb = val, val2
        if ra >= 0x80000000 then ra = ra - 0x100000000 end
        if rb >= 0x80000000 then rb = rb - 0x100000000 end
        self.reg[%d] = val
        self.mem32[addr / 4] = ra > rb and val or val2
    ]]):format(pc, inst.rs1, inst.rs2, inst.rd)
end

RISCV.atomic_opcodes[24] = function(pc, inst) -- AMOMINU.W
    return ([[
        -- %08X: AMOMINU.W
        local addr = self.reg[%d]
        if addr %% 4 ~= 0 then error("unaligned AMO instruction") end
        local val, val2 = self.mem32[addr / 4], self.reg[%d]
        self.reg[%d] = val
        self.mem32[addr / 4] = val < val2 and val or val2
    ]]):format(pc, inst.rs1, inst.rs2, inst.rd)
end

RISCV.atomic_opcodes[28] = function(pc, inst) -- AMOMAXU.W
    return ([[
        -- %08X: AMOMAXU.W
        local addr = self.reg[%d]
        if addr %% 4 ~= 0 then error("unaligned AMO instruction") end
        local val, val2 = self.mem32[addr / 4], self.reg[%d]
        self.reg[%d] = val
        self.mem32[addr / 4] = val > val2 and val or val2
    ]]):format(pc, inst.rs1, inst.rs2, inst.rd)
end

RISCV.traces = setmetatable({}, {__index = function(trace, pc)
    local self = RISCV
    local base = pc
    local chunk = ([[
    local math, bit32 = math, bit32
    return function(self)
        if self.halt then return end
        self.branches = self.branches + 1
        if self.branches > self.branchesLimit then coroutine.yield() end
        self.pc = %d
    ]]):format(pc)
    repeat
        if pc >= 33554432 then error("pc out of bounds") end
        if pc % 4 ~= 0 then error(("unaligned jump to %08X"):format(pc)) end
        local inst = self.mem32[pc / 4]
        if inst == 0xc0001073 then chunk = chunk .. "self.halt = true\n" break end
        pc = pc + 4
        local mode = opcode_modes[bit32.band(inst, 0x7F)]
        if not mode then
            error(("Unknown opcode %02X at %08X"):format(bit32.band(inst, 0x7F), pc - 4))
            return
        end
        inst = mode(inst)
        --print(textutils.serialize(inst))
        local f = self.opcodes[inst.opcode]
        local op, isBranch
        if type(f) == "function" then op, isBranch = f(pc, inst)
        elseif type(f) == "table" then
            if not f[inst.funct3] then print("Unknown function " .. inst.funct3)
            elseif inst.opcode == 0x33 and bit32.btest(inst.funct7, 1) then op, isBranch = self.mult_opcodes[inst.funct3](pc, inst)
            elseif inst.opcode == 0x2F and inst.funct3 == 2 then op, isBranch = self.atomic_opcodes[bit32.rshift(inst.funct7, 2)](pc, inst)
            else op, isBranch = f[inst.funct3](pc, inst) end
        else error("Unknown opcode " .. inst.opcode .. " at " .. (pc - 4)) end
        chunk = chunk .. op
    until isBranch
    chunk = chunk .. "end"
    --print(chunk)
    local fn = assert(load(chunk, ("@%08X"):format(base)))()
    trace[base] = fn
    return fn
end})

function RISCV:run(cycles)
    self.branches = 0
    self.branchesLimit = cycles
    if self.coro then
        assert(coroutine.resume(self.coro))
    else
        self.coro = coroutine.create(self.traces[self.pc])
        assert(coroutine.resume(self.coro, self))
    end
    if coroutine.status(self.coro) == "dead" then self.coro = nil end
end

function RISCV:call(addr, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
    --print(("Call: %08x"):format(addr))
    self.coro = nil
    local oldra = self.reg[1]
    local oldreg = {}
    local oldpc = self.pc
    if arg1 then oldreg[1], self.reg[10] = self.reg[10], arg1 end
    if arg2 then oldreg[2], self.reg[11] = self.reg[11], arg2 end
    if arg3 then oldreg[3], self.reg[12] = self.reg[12], arg3 end
    if arg4 then oldreg[4], self.reg[13] = self.reg[13], arg4 end
    if arg5 then oldreg[5], self.reg[14] = self.reg[14], arg5 end
    if arg6 then oldreg[6], self.reg[15] = self.reg[15], arg6 end
    if arg7 then oldreg[7], self.reg[16] = self.reg[16], arg7 end
    self.reg[1] = oldpc
    self.pc = addr
    local oldtrace = rawget(self.traces, oldpc)
    self.traces[oldpc] = function() self.pc = oldpc end
    while self.pc ~= oldpc do if self.halt then return end self:run(1) end
    self.traces[oldpc] = oldtrace
    local res = self.reg[10]
    self.reg[1] = oldra
    for k, v in pairs(oldreg) do self.reg[k+9] = v end
    return res
end

if not _G._TRACEBACK then
    _G.pcall = function(f, ...) return xpcall(f, debug.traceback, ...) end
    local resume = coroutine.resume
    function coroutine.resume(coro, ...)
        local res = table.pack(resume(coro, ...))
        if not res[1] then res[2] = debug.traceback(coro, res[2]) end
        return table.unpack(res, 1, res.n)
    end
    _G._TRACEBACK = true
end

local luastate = require "luastate"
local dynload = require "dynload"

local function resolveSymbol(addr, modules)
    local sym, symaddr = "??", 0
    for k, m in pairs(modules) do
        for _, t in pairs(m.elf.sections) do
            if t.symbols then
                for _, s in ipairs(t.symbols) do
                    if s.type == "FUNC" and addr >= m.baseAddress + s.value and (m.baseAddress + s.value > symaddr or (m.baseAddress + s.value == symaddr and s.binding ~= "LOCAL")) then
                        sym, symaddr = k .. ":" .. s.name, m.baseAddress + s.value
                    end
                end
            end
        end
    end
    return sym, symaddr
end

local function dump(cpu, modules, endAddr)
    local msg = ""
    do
        local sym, symaddr = resolveSymbol(cpu.pc, modules)
        msg = msg .. ("\npc=%08x (%s+%x)\n"):format(cpu.pc, sym, cpu.pc - symaddr)
    end
    for y = 0, 31, 4 do
        for x = 0, 3 do
            msg = msg .. ("x%d=%08x "):format(y+x, cpu.reg[y+x])
        end
        msg = msg .. "\n"
    end
    msg = msg .. "\nLoaded modules:\n"
    for k, v in pairs(modules) do msg = msg .. ("%08x  %s\n"):format(v.baseAddress, k) end
    msg = msg .. "\nStack trace (estimate):\n"
    do
        local sym, symaddr = resolveSymbol(cpu.pc, modules)
        msg = msg .. ("%08x  %s+%x\n"):format(cpu.pc, sym, cpu.pc - symaddr)
    end
    do
        local sym, symaddr = resolveSymbol(cpu.reg[1], modules)
        msg = msg .. ("%08x  %s+%x\n"):format(cpu.reg[1], sym, cpu.reg[1] - symaddr)
    end
    for sp = cpu.reg[2] / 4, 0x7FBFFF do
        local v = cpu.mem32[sp]
        if v > 0 and v < endAddr and v % 4 == 0 then
            local inst = cpu.mem32[v / 4 - 1]
            local op = bit32.band(inst, 0x7F)
            if op == 0x6F or op == 0x67 then
                inst = opcode_modes[op](inst)
                if inst.rd == 1 then
                    local sym, symaddr = resolveSymbol(v, modules)
                    msg = msg .. ("%08x  %s+%x\n"):format(v, sym, v - symaddr)
                end
            end
        end
    end
    return msg
end

local function loadmodule(name)
    RISCV.reg[1] = 0x1FFFFFC -- return address (trap)
    RISCV.mem32[0x7FFFFF] = 0x00100073 -- EBREAK
    RISCV.reg[2] = 0x1FF0000 -- stack pointer
    -- thread pointer setup
    RISCV.reg[4] = 0x2000800
    RISCV.mem32[0x8001FE] = 0x2000800
    RISCV.mem32[0x8001FF] = 0
    RISCV.mem32[0x800200] = 0
    RISCV.modules = {}
    RISCV.endAddr = dynload(RISCV, RISCV.modules, name, 0)
    local entrypoint
    --for k, v in pairs(modules) do print(("%08x"):format(v.baseAddress), k) end
    for k, v in pairs(RISCV.modules[name].symbols) do if k:match("^luaopen_") then entrypoint = v end end
    if RISCV.modules["libc.so.6"] and RISCV.modules["libc.so.6"].symbols["__libc_early_init"] then
        local ok, err = xpcall(function()
            RISCV:call(RISCV.modules["libc.so.6"].symbols["__libc_early_init"], 1)
        end, function(msg)
            return msg .. "\n" .. dump(RISCV, RISCV.modules, RISCV.endAddr)
        end)
        if not ok then error(err) end
    end
    local nres
    local state = luastate.call_state(RISCV, luastate.cclosure(entrypoint), "test")
    local ok, err = xpcall(function()
        nres = RISCV:call(entrypoint, state)
    end, function(msg)
        return msg .. "\n" .. dump(RISCV, RISCV.modules, RISCV.endAddr)
    end)
    if not ok then error(err) end
    local st = luastate.states[state].stack
    local res = {table.unpack(st, st.n - nres + 1, st.n)}
    for i = 1, nres do res[i] = luastate.lua_value(res[i], RISCV) end
    return table.unpack(res, 1, nres)
end

if ... ~= "riscv" then
    local name, fn = ...
    local test = loadmodule(name)
    local ok, err = xpcall(function(...)
        print(test[fn](...))
        --print(test.crc32()(os.about()))
    end, function(msg)
        return msg .. "\n" .. dump(RISCV, RISCV.modules, RISCV.endAddr)
    end, select(3, ...))
    if not ok then error(err) end
    return
end

return loadmodule
