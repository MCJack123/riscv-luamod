-- Licensed under GPLv2
if not ffi then error("Requires FFI library on LuaJIT, enable jit_ffi_enable in config/global.json") end
local RISCV = {reg = {}, pc = 0, syscalls = require "syscall", opcodes = {[0x63] = {}, [0x03] = {}, [0x23] = {}, [0x13] = {}, [0x33] = {}, [0x2F] = {[2] = true}}, mult_opcodes = {}, atomic_opcodes = {}, atomic_rs = {}, halt = false, sysdata = {}}
for i = 1, 31 do RISCV.reg[i] = 0 end
setmetatable(RISCV.reg, {__index = function() return 0 end, __newindex = function() end})
RISCV.mem = ffi.new("uint8_t[?]", 0x2010000)
RISCV.mem16 = ffi.cast("uint16_t*", RISCV.mem)
RISCV.mem32 = ffi.cast("uint32_t*", RISCV.mem)

local function signed(num)
    if bit32.btest(num, 0x80000000) then return num - 0x100000000 end
    return num
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

RISCV.opcodes[0x37] = function(self, inst) -- LUI
    self.reg[inst.rd] = inst.imm
    return "LUI x" .. inst.rd .. ", " .. inst.imm
end

RISCV.opcodes[0x17] = function(self, inst) -- AUIPC
    self.reg[inst.rd] = (self.pc - 4 + inst.imm) % 0x100000000
    return "AUIPC x" .. inst.rd .. ", " .. inst.imm
end

RISCV.opcodes[0x6F] = function(self, inst) -- JAL
    self.reg[inst.rd] = self.pc
    self.pc = self.pc + inst.simm - 4
    if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    return "JAL x" .. inst.rd .. ", " .. inst.simm
end

RISCV.opcodes[0x67] = function(self, inst) -- JALR
    local oldpc = self.pc
    self.pc = bit32.band(self.reg[inst.rs1] + inst.simm, 0xFFFFFFFE)
    self.reg[inst.rd] = oldpc
    if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    return "JALR x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x63][0] = function(self, inst) -- BEQ
    if self.reg[inst.rs1] == self.reg[inst.rs2] then
        self.pc = self.pc + inst.simm - 4
        if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    end
    return "BEQ x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x63][1] = function(self, inst) -- BNE
    if self.reg[inst.rs1] ~= self.reg[inst.rs2] then
        self.pc = self.pc + inst.simm - 4
        if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    end
    return "BNE x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x63][4] = function(self, inst) -- BLT
    if signed(self.reg[inst.rs1]) < signed(self.reg[inst.rs2]) then
        self.pc = self.pc + inst.simm - 4
        if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    end
    return "BLT x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x63][5] = function(self, inst) -- BGE
    if signed(self.reg[inst.rs1]) >= signed(self.reg[inst.rs2]) then
        self.pc = self.pc + inst.simm - 4
        if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    end
    return "BGE x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x63][6] = function(self, inst) -- BLTU
    if self.reg[inst.rs1] < self.reg[inst.rs2] then
        self.pc = self.pc + inst.simm - 4
        if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    end
    return "BLTU x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x63][7] = function(self, inst) -- BGEU
    if self.reg[inst.rs1] >= self.reg[inst.rs2] then
        self.pc = self.pc + inst.simm - 4
        if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    end
    return "BGEU x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x03][0] = function(self, inst) -- LB
    local addr = self.reg[inst.rs1] + inst.simm
    if addr < 0 then addr = addr + 0x100000000 end
    if addr > ffi.sizeof(self.mem) then error(("out of bounds read at %08x"):format(addr)) end
    self.reg[inst.rd] = self.mem[self.reg[inst.rs1] + inst.simm]
    if bit32.btest(self.reg[inst.rd], 0x80) then self.reg[inst.rd] = bit32.bor(self.reg[inst.rd], 0xFFFFFF00) end
    return "LB x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x03][1] = function(self, inst) -- LH
    local addr = self.reg[inst.rs1] + inst.simm
    if addr > ffi.sizeof(self.mem) then error(("out of bounds read at %08x"):format(addr)) end
    if addr % 2 ~= 0 then self.reg[inst.rd] = self.mem[addr] + self.mem[addr+1] * 256
    else self.reg[inst.rd] = self.mem16[addr / 2] end
    if bit32.btest(self.reg[inst.rd], 0x8000) then self.reg[inst.rd] = bit32.bor(self.reg[inst.rd], 0xFFFF0000) end
    return "LH x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x03][2] = function(self, inst) -- LW
    local addr = self.reg[inst.rs1] + inst.simm
    if addr < 0 then addr = addr + 0x100000000 end
    if addr > ffi.sizeof(self.mem) then error(("out of bounds read at %08x"):format(addr)) end
    if addr % 4 ~= 0 then self.reg[inst.rd] = self.mem[addr] + self.mem[addr+1] * 256 + self.mem[addr+2] * 65536 + self.mem[addr+3] * 16777216
    else self.reg[inst.rd] = self.mem32[addr / 4] end
    return "LW x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x03][4] = function(self, inst) -- LBU
    local addr = self.reg[inst.rs1] + inst.simm
    if addr < 0 then addr = addr + 0x100000000 end
    if addr > ffi.sizeof(self.mem) then error(("out of bounds read at %08x"):format(addr)) end
    self.reg[inst.rd] = self.mem[addr]
    return "LBU x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x03][5] = function(self, inst) -- LHU
    local addr = self.reg[inst.rs1] + inst.simm
    if addr < 0 then addr = addr + 0x100000000 end
    if addr > ffi.sizeof(self.mem) then error(("out of bounds read at %08x"):format(addr)) end
    if addr % 2 ~= 0 then self.reg[inst.rd] = self.mem[addr] + self.mem[addr+1] * 256
    else self.reg[inst.rd] = self.mem16[addr / 2] end
    return "LHU x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x23][0] = function(self, inst) -- SB
    local addr = self.reg[inst.rs1] + inst.simm
    if addr < 0 then addr = addr + 0x100000000 end
    if addr > ffi.sizeof(self.mem) then error(("out of bounds write at %08x"):format(addr)) end
    self.mem[addr] = bit32.band(self.reg[inst.rs2], 0xFF)
    self.atomic_rs[math.floor(addr / 4)] = nil
    return "SB x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x23][1] = function(self, inst) -- SH
    local addr = self.reg[inst.rs1] + inst.simm
    if addr < 0 then addr = addr + 0x100000000 end
    if addr > ffi.sizeof(self.mem) then error(("out of bounds write at %08x"):format(addr)) end
    if addr % 2 ~= 0 then
        self.mem[addr] = bit32.extract(self.reg[inst.rs2], 0, 8)
        self.mem[addr+1] = bit32.extract(self.reg[inst.rs2], 8, 8)
    else self.mem16[addr / 2] = bit32.band(self.reg[inst.rs2], 0xFFFF) end
    self.atomic_rs[math.floor(addr / 4)] = nil
    self.atomic_rs[math.floor((addr + 1) / 4)] = nil
    return "SH x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x23][2] = function(self, inst) -- SW
    local addr = self.reg[inst.rs1] + inst.simm
    if addr < 0 then addr = addr + 0x100000000 end
    if addr > ffi.sizeof(self.mem) then error(("out of bounds write at %08x"):format(addr)) end
    if addr % 4 ~= 0 then
        self.mem[addr] = bit32.extract(self.reg[inst.rs2], 0, 8)
        self.mem[addr+1] = bit32.extract(self.reg[inst.rs2], 8, 8)
        self.mem[addr+2] = bit32.extract(self.reg[inst.rs2], 16, 8)
        self.mem[addr+3] = bit32.extract(self.reg[inst.rs2], 24, 8)
    else self.mem32[addr / 4] = self.reg[inst.rs2] end
    self.atomic_rs[math.floor(addr / 4)] = nil
    self.atomic_rs[math.floor((addr + 3) / 4)] = nil
    return "SW x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x13][0] = function(self, inst) -- ADDI
    local v = (self.reg[inst.rs1] + inst.simm) % 0x100000000
    if v < 0 then v = v + 0x100000000 end
    self.reg[inst.rd] = v
    return "ADDI x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x13][2] = function(self, inst) -- SLTI
    self.reg[inst.rd] = signed(self.reg[inst.rs1]) < inst.simm and 1 or 0
    return "SLTI x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.imm
end

RISCV.opcodes[0x13][3] = function(self, inst) -- SLTIU
    local imm = inst.simm
    if imm < 0 then imm = imm + 0x100000000 end
    self.reg[inst.rd] = self.reg[inst.rs1] < imm and 1 or 0
    return "SLTIU x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.imm
end

RISCV.opcodes[0x13][4] = function(self, inst) -- XORI
    self.reg[inst.rd] = bit32.bxor(self.reg[inst.rs1], inst.simm)
    return "XORI x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x13][6] = function(self, inst) -- ORI
    self.reg[inst.rd] = bit32.bor(self.reg[inst.rs1], inst.simm)
    return "ORI x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x13][7] = function(self, inst) -- ANDI
    self.reg[inst.rd] = bit32.band(self.reg[inst.rs1], inst.simm)
    return "ANDI x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x13][1] = function(self, inst) -- SLLI
    self.reg[inst.rd] = bit32.lshift(self.reg[inst.rs1], bit32.band(inst.imm, 0x1F))
    return "SLLI x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.imm
end

RISCV.opcodes[0x13][5] = function(self, inst) -- SRLI/SRAI
    self.reg[inst.rd] = (bit32.btest(inst.imm, 0x400) and bit32.arshift or bit32.rshift)(self.reg[inst.rs1], bit32.band(inst.imm, 0x1F))
    return "SRLI/SRAI x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.imm
end

RISCV.opcodes[0x33][0] = function(self, inst) -- ADD/SUB
    if bit32.btest(inst.funct7, 0x20) then self.reg[inst.rd] = (self.reg[inst.rs1] - self.reg[inst.rs2]) % 0x100000000
    else self.reg[inst.rd] = (self.reg[inst.rs1] + self.reg[inst.rs2]) % 0x100000000 end
    return "ADD/SUB x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.opcodes[0x33][1] = function(self, inst) -- SLL
    self.reg[inst.rd] = bit32.lshift(self.reg[inst.rs1], bit32.band(self.reg[inst.rs2], 0x1F))
    return "SLL x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.opcodes[0x33][2] = function(self, inst) -- SLT
    self.reg[inst.rd] = signed(self.reg[inst.rs1]) < signed(self.reg[inst.rs2]) and 1 or 0
    return "SLT x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.opcodes[0x33][3] = function(self, inst) -- SLTU
    self.reg[inst.rd] = self.reg[inst.rs1] < self.reg[inst.rs2] and 1 or 0
    return "SLTU x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.opcodes[0x33][4] = function(self, inst) -- XOR
    self.reg[inst.rd] = bit32.bxor(self.reg[inst.rs1], self.reg[inst.rs2])
    return "XOR x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.opcodes[0x33][5] = function(self, inst) -- SRL/SRA
    self.reg[inst.rd] = (bit32.btest(inst.funct7, 0x20) and bit32.arshift or bit32.rshift)(self.reg[inst.rs1], bit32.band(self.reg[inst.rs2], 0x1F))
    return "SRL/SRA x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.opcodes[0x33][6] = function(self, inst) -- OR
    self.reg[inst.rd] = bit32.bor(self.reg[inst.rs1], self.reg[inst.rs2])
    return "OR x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.opcodes[0x33][7] = function(self, inst) -- AND
    self.reg[inst.rd] = bit32.band(self.reg[inst.rs1], self.reg[inst.rs2])
    return "AND x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.opcodes[0x0F] = function(self, inst) -- FENCE
    -- do nothing, only one hart
    return "FENCE"
end

RISCV.opcodes[0x73] = function(self, inst) -- ECALL/EBREAK
    if inst.imm == 1 then self.halt = true return "EBREAK" end
    if self.syscalls[self.reg[17]] then self.reg[10] = self.syscalls[self.reg[17]](self, table.unpack(self.reg, 10, 16))
    else error("Unimplemented syscall " .. self.reg[17]) self.reg[10] = 0xFFFFFFFF end -- ENOSYS
    return "ECALL " .. self.reg[17]
end

RISCV.mult_opcodes[0] = function(self, inst) -- MUL
    self.reg[inst.rd] = math.abs((signed(self.reg[inst.rs1]) * signed(self.reg[inst.rs2])) % 0x100000000)
    return "MUL x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.mult_opcodes[3] = function(self, inst) -- MULHU
    self.reg[inst.rd] = math.floor((self.reg[inst.rs1] * self.reg[inst.rs2]) / 0x100000000)
    return "MULH x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.mult_opcodes[2] = function(self, inst) -- MULHSU
    self.reg[inst.rd] = math.floor((signed(self.reg[inst.rs1]) * self.reg[inst.rs2]) / 0x100000000)
    if self.reg[inst.rd] < 0 then self.reg[inst.rd] = self.reg[inst.rd] + 0x100000000 end
    return "MULHSU x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.mult_opcodes[1] = function(self, inst) -- MULH
    self.reg[inst.rd] = math.floor((signed(self.reg[inst.rs1]) * signed(self.reg[inst.rs2])) / 0x100000000)
    if self.reg[inst.rd] < 0 then self.reg[inst.rd] = self.reg[inst.rd] + 0x100000000 end
    return "MULHU x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.mult_opcodes[4] = function(self, inst) -- DIV
    if self.reg[inst.rs2] == 0 then
        self.reg[inst.rd] = 0xFFFFFFFF
    else
        local res = signed(self.reg[inst.rs1]) / signed(self.reg[inst.rs2])
        if res < 0 then self.reg[inst.rd] = math.ceil(res) + 0x100000000
        else self.reg[inst.rd] = math.floor(res) end
    end
    return "DIV x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.mult_opcodes[5] = function(self, inst) -- DIVU
    if self.reg[inst.rs2] == 0 then self.reg[inst.rd] = 0xFFFFFFFF
    else self.reg[inst.rd] = math.floor(self.reg[inst.rs1] / self.reg[inst.rs2]) end
    return "DIVU x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.mult_opcodes[6] = function(self, inst) -- REM
    if self.reg[inst.rs2] == 0 then
        self.reg[inst.rd] = self.reg[inst.rs1]
    else
        local res = math.fmod(signed(self.reg[inst.rs1]), signed(self.reg[inst.rs2]))
        if res < 0 then self.reg[inst.rd] = math.ceil(res) + 0x100000000
        else self.reg[inst.rd] = math.floor(res) end
    end
    return "REM x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.mult_opcodes[7] = function(self, inst) -- REMU
    if self.reg[inst.rs2] == 0 then self.reg[inst.rd] = self.reg[inst.rs1]
    else self.reg[inst.rd] = self.reg[inst.rs1] % self.reg[inst.rs2] end
    return "REMU x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.atomic_opcodes[0] = function(self, inst) -- AMOADD.W
    local addr = self.reg[inst.rs1]
    if addr > ffi.sizeof(self.mem) then error(("out of bounds read at %08x"):format(addr)) end
    if addr % 4 ~= 0 then error("unaligned AMO instruction") end
    local val = self.mem32[addr / 4]
    self.reg[inst.rd] = val
    self.mem32[addr / 4] = (val + self.reg[inst.rs2]) % 0x100000000
    return "AMOADD.W x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.atomic_opcodes[1] = function(self, inst) -- AMOSWAP.W
    local addr = self.reg[inst.rs1]
    if addr > ffi.sizeof(self.mem) then error(("out of bounds read at %08x"):format(addr)) end
    if addr % 4 ~= 0 then error("unaligned AMO instruction") end
    local val = self.mem32[addr / 4]
    self.reg[inst.rd] = val
    self.mem32[addr / 4] = self.reg[inst.rs2]
    return "AMOSWAP.W x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.atomic_opcodes[2] = function(self, inst) -- LR.W
    local addr = self.reg[inst.rs1]
    if addr > ffi.sizeof(self.mem) then error(("out of bounds read at %08x"):format(addr)) end
    if addr % 4 ~= 0 then error("unaligned AMO instruction")
    else self.reg[inst.rd] = self.mem32[addr / 4] end
    self.atomic_rs[addr / 4] = true
    return "LR.W x" .. inst.rd .. ", x" .. inst.rs1
end

RISCV.atomic_opcodes[3] = function(self, inst) -- SC.W
    local addr = self.reg[inst.rs1]
    if addr > ffi.sizeof(self.mem) then error(("out of bounds write at %08x"):format(addr)) end
    if addr % 4 ~= 0 then error("unaligned AMO instruction")
    elseif self.atomic_rs[addr / 4] then
        self.mem32[addr / 4] = self.reg[inst.rs2]
        self.reg[inst.rd] = 0
    else
        self.reg[inst.rd] = 1
    end
    self.atomic_rs[addr / 4] = nil
    return "SC.W x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.atomic_opcodes[4] = function(self, inst) -- AMOXOR.W
    local addr = self.reg[inst.rs1]
    if addr > ffi.sizeof(self.mem) then error(("out of bounds read at %08x"):format(addr)) end
    if addr % 4 ~= 0 then error("unaligned AMO instruction") end
    local val = self.mem32[addr / 4]
    self.reg[inst.rd] = val
    self.mem32[addr / 4] = bit32.bxor(val, self.reg[inst.rs2])
    return "AMOXOR.W x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.atomic_opcodes[8] = function(self, inst) -- AMOOR.W
    local addr = self.reg[inst.rs1]
    if addr > ffi.sizeof(self.mem) then error(("out of bounds read at %08x"):format(addr)) end
    if addr % 4 ~= 0 then error("unaligned AMO instruction") end
    local val = self.mem32[addr / 4]
    self.reg[inst.rd] = val
    self.mem32[addr / 4] = bit32.bor(val, self.reg[inst.rs2])
    return "AMOOR.W x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.atomic_opcodes[12] = function(self, inst) -- AMOAND.W
    local addr = self.reg[inst.rs1]
    if addr > ffi.sizeof(self.mem) then error(("out of bounds read at %08x"):format(addr)) end
    if addr % 4 ~= 0 then error("unaligned AMO instruction") end
    local val = self.mem32[addr / 4]
    self.reg[inst.rd] = val
    self.mem32[addr / 4] = bit32.band(val, self.reg[inst.rs2])
    return "AMOAND.W x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.atomic_opcodes[16] = function(self, inst) -- AMOMIN.W
    local addr = self.reg[inst.rs1]
    if addr > ffi.sizeof(self.mem) then error(("out of bounds read at %08x"):format(addr)) end
    if addr % 4 ~= 0 then error("unaligned AMO instruction") end
    local val = self.mem32[addr / 4]
    self.reg[inst.rd] = val
    self.mem32[addr / 4] = signed(val) < signed(self.reg[inst.rs2]) and val or self.reg[inst.rs2]
    return "AMOMIN.W x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.atomic_opcodes[20] = function(self, inst) -- AMOMAX.W
    local addr = self.reg[inst.rs1]
    if addr > ffi.sizeof(self.mem) then error(("out of bounds read at %08x"):format(addr)) end
    if addr % 4 ~= 0 then error("unaligned AMO instruction") end
    local val = self.mem32[addr / 4]
    self.reg[inst.rd] = val
    self.mem32[addr / 4] = signed(val) > signed(self.reg[inst.rs2]) and val or self.reg[inst.rs2]
    return "AMOMAX.W x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.atomic_opcodes[24] = function(self, inst) -- AMOMINU.W
    local addr = self.reg[inst.rs1]
    if addr > ffi.sizeof(self.mem) then error(("out of bounds read at %08x"):format(addr)) end
    if addr % 4 ~= 0 then error("unaligned AMO instruction") end
    local val = self.mem32[addr / 4]
    self.reg[inst.rd] = val
    self.mem32[addr / 4] = math.min(val, self.reg[inst.rs2])
    return "AMOMINU.W x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.atomic_opcodes[28] = function(self, inst) -- AMOMAXU.W
    local addr = self.reg[inst.rs1]
    if addr > ffi.sizeof(self.mem) then error(("out of bounds read at %08x"):format(addr)) end
    if addr % 4 ~= 0 then error("unaligned AMO instruction") end
    local val = self.mem32[addr / 4]
    self.reg[inst.rd] = val
    self.mem32[addr / 4] = math.max(val, self.reg[inst.rs2])
    return "AMOMAXU.W x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

function RISCV:clock()
    if self.pc >= 33554432 then error("pc out of bounds") end
    local oldpc = self.pc
    local inst = self.mem32[self.pc / 4]
    self.pc = self.pc + 4
    local mode = opcode_modes[bit32.band(inst, 0x7F)]
    if not mode then
        error(("Unknown opcode %02X"):format(bit32.band(inst, 0x7F)))
        return
    end
    inst = mode(inst)
    --print(textutils.serialize(inst))
    local f = self.opcodes[inst.opcode]
    local op
    if type(f) == "function" then op = f(self, inst)
    elseif type(f) == "table" then
        if not f[inst.funct3] then error("Unknown function " .. inst.funct3)
        elseif inst.opcode == 0x33 and bit32.btest(inst.funct7, 1) then op = self.mult_opcodes[inst.funct3](self, inst)
        elseif inst.opcode == 0x2F and inst.funct3 == 2 then op = self.atomic_opcodes[bit32.rshift(inst.funct7, 2)](self, inst)
        else op = f[inst.funct3](self, inst) end
    else error("Unknown opcode " .. inst.opcode) end
    --if op then print(("%08x  %s"):format(oldpc, op)) end
end

function RISCV:run(cycles)
    for i = 1, cycles do if self.halt then return end self:clock() --[[sleep(0.05)]] end
end

function RISCV:call(addr, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
    --print(("Call: %08x"):format(addr))
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
    while self.pc ~= oldpc do if self.halt then return end self:clock() end
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
