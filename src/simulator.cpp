#include "simulator.h"

#include <iostream>
#include <stdexcept>
#include <stdio.h>
using namespace std;

Simulator::Simulator() {
    // Initialize member variables
    memory = nullptr;
    regData.reg = {};
    din = 0;
}

Simulator::~Simulator() {
    if (memory)
        delete memory;
}

// dump registers and memory
void Simulator::dumpRegMem(const std::string &output_name) {
    dumpRegisterState(regData.reg, output_name);
    dumpMemoryState(memory, output_name);
}

// Get raw instruction bits from memory
Simulator::Instruction Simulator::simFetch(uint64_t PC, MemoryStore *myMem) {
    // fetch current instruction
    uint64_t instruction;
    myMem->getMemValue(PC, instruction, WORD_SIZE);
    instruction = (uint32_t)instruction;

    Instruction inst;
    inst.PC = PC;
    inst.instruction = instruction;
    return inst;
}

// Determine instruction opcode, funct, reg names (but not calculate all imms)
Simulator::Instruction Simulator::simDecode(Instruction inst) {
    inst.opcode = extractBits(inst.instruction, 6, 0);
    inst.rd = extractBits(inst.instruction, 11, 7);
    inst.funct3 = extractBits(inst.instruction, 14, 12);
    inst.rs1 = extractBits(inst.instruction, 19, 15);
    inst.rs2 = extractBits(inst.instruction, 24, 20);
    inst.funct7 = extractBits(inst.instruction, 31, 25);

    inst.isLegal = true; // assume legal unless proven otherwise

    if (inst.instruction == 0xfeedfeed) {
        inst.isHalt = true;
        return inst; // halt instruction
    }
    if (inst.instruction == 0x00000013) {
        inst.isNop = true;
        return inst; // NOP instruction
    }

    switch (inst.opcode) {
    case OP_INT:
        if ((inst.funct3 == FUNCT3_ADD &&
             (inst.funct7 == FUNCT7_ADD || inst.funct7 == FUNCT7_SUB)) ||
            inst.funct3 == FUNCT3_SLL || inst.funct3 == FUNCT3_SLT ||
            inst.funct3 == FUNCT3_SLTU || inst.funct3 == FUNCT3_XOR ||
            (inst.funct3 == FUNCT3_SR &&
             (inst.funct7 >> 1 == UPPERIMM_ARITH ||
              inst.funct7 >> 1 == UPPERIMM_LOGICAL)) ||
            inst.funct3 == FUNCT3_OR || inst.funct3 == FUNCT3_AND) {
            inst.doesArithLogic = true;
            inst.writesRd = true;
            inst.readsRs1 = true;
            inst.readsRs2 = true;
        } else {
            inst.isLegal = false;
        }
        break;
    case OP_INTW:
        if ((inst.funct3 == FUNCT3_ADD &&
             (inst.funct7 == FUNCT7_ADD || inst.funct7 == FUNCT7_SUB)) ||
            inst.funct3 == FUNCT3_SLL ||
            (inst.funct3 == FUNCT3_SR &&
             (inst.funct7 == FUNCT7_ARITH || inst.funct7 == FUNCT7_LOGICAL))) {
            inst.doesArithLogic = true;
            inst.writesRd = true;
            inst.readsRs1 = true;
            inst.readsRs2 = true;
        } else {
            inst.isLegal = false;
        }
        break;
    case OP_LOAD:
        if (inst.funct3 == FUNCT3_B || inst.funct3 == FUNCT3_H ||
            inst.funct3 == FUNCT3_W || inst.funct3 == FUNCT3_D ||
            inst.funct3 == FUNCT3_BU || inst.funct3 == FUNCT3_HU ||
            inst.funct3 == FUNCT3_WU) {
            inst.readsRs1 = true;
            inst.writesRd = true;
            inst.readsMem = true;
        } else {
            inst.isLegal = false;
        }
        break;
    case OP_INTIMM:
        if (inst.funct3 == FUNCT3_ADD || inst.funct3 == FUNCT3_SLL ||
            inst.funct3 == FUNCT3_SLT || inst.funct3 == FUNCT3_SLTU ||
            inst.funct3 == FUNCT3_XOR ||
            (inst.funct3 == FUNCT3_SR &&
             (inst.funct7 >> 1 == UPPERIMM_ARITH ||
              inst.funct7 >> 1 == UPPERIMM_LOGICAL)) ||
            inst.funct3 == FUNCT3_OR || inst.funct3 == FUNCT3_AND) {
            inst.doesArithLogic = true;
            inst.writesRd = true;
            inst.readsRs1 = true;
        } else {
            inst.isLegal = false;
        }
        break;
    case OP_INTIMMW:
        if (inst.funct3 == FUNCT3_ADD || inst.funct3 == FUNCT3_SLL ||
            (inst.funct3 == FUNCT3_SR &&
             (inst.funct7 == FUNCT7_ARITH || inst.funct7 == FUNCT7_LOGICAL))) {
            inst.doesArithLogic = true;
            inst.writesRd = true;
            inst.readsRs1 = true;
        } else {
            inst.isLegal = false;
        }
        break;
    case OP_JALR:
        inst.isLegal = true;
        inst.doesArithLogic = true;
        inst.writesRd = true;
        inst.readsRs1 = true;
        break;
    case OP_STORE:
        if (inst.funct3 == FUNCT3_B || inst.funct3 == FUNCT3_H ||
            inst.funct3 == FUNCT3_W || inst.funct3 == FUNCT3_D) {
            inst.readsRs1 = true;
            inst.readsRs2 = true;
            inst.writesMem = true;
        } else {
            inst.isLegal = false;
        }
        break;
    case OP_BRANCH:
        if (inst.funct3 == FUNCT3_BEQ || inst.funct3 == FUNCT3_BNE ||
            inst.funct3 == FUNCT3_BLT || inst.funct3 == FUNCT3_BGE ||
            inst.funct3 == FUNCT3_BLTU || inst.funct3 == FUNCT3_BGEU) {
            inst.readsRs1 = true;
            inst.readsRs2 = true;
        } else {
            inst.isLegal = false;
        }
        break;
    case OP_AUIPC:
    case OP_LUI:
    case OP_JAL:
        inst.isLegal = true;
        inst.doesArithLogic = true;
        inst.writesRd = true;
        break;
    default:
        inst.isLegal = false;
    }
    return inst;
}

// Collect operands whether reg or imm for arith or addr gen
Simulator::Instruction Simulator::simOperandCollection(Instruction inst,
                                                       REGS regData) {
    regData.registers[0] = 0;
    if (inst.readsRs1) {
        inst.op1Val = regData.registers[inst.rs1];
    }
    if (inst.readsRs2) {
        inst.op2Val = regData.registers[inst.rs2];
    }
    return inst;
}

// Resolve next PC whether +4 or branch/jump target taken/not taken
Simulator::Instruction Simulator::simNextPCResolution(Instruction inst) {

    uint64_t imm5 = inst.rd;
    uint64_t imm7 = inst.funct7;
    uint64_t imm12 = extractBits(inst.instruction, 31, 20);
    uint64_t imm20 = extractBits(inst.instruction, 31, 12);
    uint64_t branchTarget =
        inst.PC +
        sext64(extractBits(imm7, 6, 6) << 12 | extractBits(imm7, 5, 0) << 5 |
                   extractBits(imm5, 4, 1) << 1 | extractBits(imm5, 0, 0) << 11,
               12); // B-type immediate
    uint64_t jalTarget = inst.PC + sext64(extractBits(imm20, 19, 19) << 20 |
                                              extractBits(imm20, 18, 9) << 1 |
                                              extractBits(imm20, 8, 8) << 11 |
                                              extractBits(imm20, 7, 0) << 12,
                                          20); // J-type immediate

    switch (inst.opcode) {
    case OP_JALR:
        inst.nextPC = (inst.op1Val + sext64(imm12, 11)) & ~1ULL;
        break;
    case OP_BRANCH:
        inst.nextPC = inst.PC + 4;
        switch (inst.funct3) {
        case FUNCT3_BEQ:
            if (inst.op1Val == inst.op2Val) {
                inst.nextPC = branchTarget;
            }
            break;
        case FUNCT3_BNE:
            if (inst.op1Val != inst.op2Val) {
                inst.nextPC = branchTarget;
            }
            break;
        case FUNCT3_BLT:
            std::cout << "blt check: op1Val[x" << inst.rs1
                      << "]=" << inst.op1Val << ", op2Val[x" << inst.rs2
                      << "]=" << inst.op2Val << "\n";
            if ((int64_t)inst.op1Val < (int64_t)inst.op2Val) {
                inst.nextPC = branchTarget;
            }
            break;
        case FUNCT3_BGE:
            if ((int64_t)inst.op1Val >= (int64_t)inst.op2Val) {
                inst.nextPC = branchTarget;
            }
            break;
        case FUNCT3_BLTU:
            if (inst.op1Val < inst.op2Val) {
                inst.nextPC = branchTarget;
            }
            break;
        case FUNCT3_BGEU:
            if (inst.op1Val >= inst.op2Val) {
                inst.nextPC = branchTarget;
            }
            break;
        }
        break;
    case OP_JAL:
        inst.nextPC = jalTarget;
        break;
    default:
        inst.nextPC = inst.PC + 4;
    }

    return inst;
}

// Perform arithmetic operations
Simulator::Instruction Simulator::simArithLogic(Instruction inst) {
    uint64_t imm12 = extractBits(inst.instruction, 31, 20);
    uint64_t upperImm12 = extractBits(inst.instruction, 31, 26);
    uint64_t imm20 = extractBits(inst.instruction, 31, 12);

    if (inst.opcode == OP_INT &&
        (inst.funct3 == FUNCT3_SLL || inst.funct3 == FUNCT3_SR)) {
        // For SLL and SR, only low 6 bits of rs2 are considered in
        // RV64I, so we mask it to 6 bits
        inst.op2Val &= 0x3F;
    } else if (inst.opcode == OP_INTW &&
               (inst.funct3 == FUNCT3_SLL || inst.funct3 == FUNCT3_SR)) {
        // For SLLW and SRW, only low 5 bits of rs2 are considered in
        // RV64I, so we mask it to 5 bits
        inst.op2Val &= 0x1F;
    }

    switch (inst.opcode) {
    case OP_INT:
        switch (inst.funct3) {
        case FUNCT3_ADD:
            if (inst.funct7 == FUNCT7_ADD) {
                inst.arithResult = inst.op1Val + inst.op2Val;
            } else if (inst.funct7 == FUNCT7_SUB) {
                inst.arithResult = inst.op1Val - inst.op2Val;
            }
            break;
        case FUNCT3_SLL:
            inst.arithResult = inst.op1Val << inst.op2Val;
            break;
        case FUNCT3_SLT:
            inst.arithResult = (int64_t)inst.op1Val < (int64_t)inst.op2Val;
            break;
        case FUNCT3_SLTU:
            inst.arithResult = inst.op1Val < inst.op2Val;
            break;
        case FUNCT3_XOR:
            inst.arithResult = inst.op1Val ^ inst.op2Val;
            break;
        case FUNCT3_SR:
            if (upperImm12 == FUNCT7_LOGICAL) {
                inst.arithResult = inst.op1Val >> inst.op2Val;
            } else if (upperImm12 == UPPERIMM_ARITH) {
                inst.arithResult = (int64_t)inst.op1Val >> inst.op2Val;
            }
            break;
        case FUNCT3_OR:
            inst.arithResult = inst.op1Val | inst.op2Val;
            break;
        case FUNCT3_AND:
            inst.arithResult = inst.op1Val & inst.op2Val;
            break;
        default:
            fprintf(stderr, "\tIllegal funct3\n");
        }
        break;
    case OP_INTW:
        switch (inst.funct3) {
        case FUNCT3_ADD:
            if (inst.funct7 == FUNCT7_ADD) {
                inst.arithResult =
                    sext64((uint32_t)inst.op1Val + (uint32_t)inst.op2Val, 31);
            } else if (inst.funct7 == FUNCT7_SUB) {
                inst.arithResult =
                    sext64((uint32_t)inst.op1Val - (uint32_t)inst.op2Val, 31);
            }
            break;
        case FUNCT3_SLL:
            inst.arithResult =
                sext64((uint32_t)inst.op1Val << (uint32_t)inst.op2Val, 31);
            break;
        case FUNCT3_SR:
            if (upperImm12 == FUNCT7_LOGICAL) {
                inst.arithResult =
                    sext64((uint32_t)inst.op1Val >> (uint32_t)inst.op2Val, 31);
            } else if (upperImm12 == UPPERIMM_ARITH) {
                inst.arithResult =
                    sext64((int32_t)inst.op1Val >> (uint32_t)inst.op2Val, 31);
            }
            break;
        }
        break;
    case OP_INTIMM:
        switch (inst.funct3) {
        case FUNCT3_ADD:
            inst.arithResult = inst.op1Val + sext64(imm12, 11);
            break;
        case FUNCT3_SLL:
            inst.arithResult = inst.op1Val << (imm12 & 0x3F);
            break;
        case FUNCT3_SLT:
            inst.arithResult =
                (int64_t)inst.op1Val < (int64_t)sext64(imm12, 11);
            break;
        case FUNCT3_SLTU:
            inst.arithResult = inst.op1Val < sext64(imm12, 11);
            break;
        case FUNCT3_XOR:
            inst.arithResult = inst.op1Val ^ sext64(imm12, 11);
            break;
        case FUNCT3_SR:
            if (upperImm12 == UPPERIMM_LOGICAL) {
                inst.arithResult = inst.op1Val >> (imm12 & 0x3F);
            } else if (upperImm12 == UPPERIMM_ARITH) {
                inst.arithResult = (int64_t)inst.op1Val >> (imm12 & 0x3F);
            }
            break;
        case FUNCT3_OR:
            inst.arithResult = inst.op1Val | sext64(imm12, 11);
            break;
        case FUNCT3_AND:
            inst.arithResult = inst.op1Val & sext64(imm12, 11);
            break;
        }
        break;
    case OP_INTIMMW:
        switch (inst.funct3) {
        case FUNCT3_ADD:
            inst.arithResult =
                sext64((uint32_t)inst.op1Val + (uint32_t)sext32(imm12, 11), 31);
            break;
        case FUNCT3_SLL:
            inst.arithResult =
                sext64((uint32_t)inst.op1Val << (uint32_t)(imm12 & 0x1F), 31);
            break;
        case FUNCT3_SR:
            if (upperImm12 == UPPERIMM_LOGICAL) {
                inst.arithResult = sext64(
                    (uint32_t)inst.op1Val >> (uint32_t)(imm12 & 0x1F), 31);
            } else if (upperImm12 == UPPERIMM_ARITH) {
                inst.arithResult = sext64(
                    (int32_t)inst.op1Val >> (uint32_t)(imm12 & 0x1F), 31);
            }
            break;
        }
        break;
    case OP_JALR:
        inst.arithResult = inst.PC + 4;
        break;
    case OP_AUIPC:
        inst.arithResult = inst.PC + sext64(imm20 << 12, 31);
        break;
    case OP_LUI:
        inst.arithResult = sext64(imm20 << 12, 31);
        break;
    case OP_JAL:
        inst.arithResult = inst.PC + 4;
        break;
    }

    return inst;
}

// Generate memory address for load/store instructions
Simulator::Instruction Simulator::simAddrGen(Instruction inst) {
    uint64_t imm5 = inst.rd;
    uint64_t imm7 = inst.funct7;
    uint64_t imm12 = extractBits(inst.instruction, 31, 20);
    int64_t storeImm = sext64((imm7 << 5) | imm5, 11); // S-type immediate

    if (inst.readsMem) {
        inst.memAddress = inst.op1Val + sext64(imm12, 11);
    } else if (inst.writesMem) {
        inst.memAddress = inst.op1Val + storeImm;
    }

    return inst;
}

// Perform memory access for load/store instructions
Simulator::Instruction Simulator::simMemAccess(Instruction inst,
                                               MemoryStore *myMem) {
    MemEntrySize size =
        (inst.funct3 == FUNCT3_B || inst.funct3 == FUNCT3_BU)   ? BYTE_SIZE
        : (inst.funct3 == FUNCT3_H || inst.funct3 == FUNCT3_HU) ? HALF_SIZE
        : (inst.funct3 == FUNCT3_W || inst.funct3 == FUNCT3_WU) ? WORD_SIZE
                                                                : DOUBLE_SIZE;

    if (inst.readsMem) {
        uint64_t loadedValue = 0;

        int memStatus = myMem->getMemValue(inst.memAddress, loadedValue, size);
        if (memStatus != 0) {

            inst.memException = true;
            return inst; // do NOT update memResult
        }

        if (inst.funct3 == FUNCT3_B || inst.funct3 == FUNCT3_H ||
            inst.funct3 == FUNCT3_W) {
            inst.memResult = sext64(loadedValue, size * 8 - 1);
        } else {
            inst.memResult = loadedValue;
        }
    }

    else if (inst.writesMem) {
        int memStatus = myMem->setMemValue(inst.memAddress, inst.op2Val, size);
        if (memStatus != 0) {
            inst.memException = true;
            return inst; // write did NOT happen
        }
    }
    return inst;
}

// Write back results to registers
Simulator::Instruction Simulator::simCommit(Instruction inst, REGS &regData) {
    if (inst.readsMem) {
        regData.registers[inst.rd] = inst.memResult;
        std::cout << "WB: Write to x" << inst.rd << " value " << inst.memResult
                  << "\n";
    } else {
        regData.registers[inst.rd] = inst.arithResult;
        std::cout << "WB: Write to x" << inst.rd << " value "
                  << inst.arithResult << "\n";
    }
    return inst;
}

// TODO complete the following pipeline stage simulation functions
// You may find it useful to call functional simulation functions above

Simulator::Instruction Simulator::simIF(uint64_t PC) {
    Instruction inst = simFetch(PC, memory);
    // inst.status = NORMAL;
    return inst; // TODO implement IF
}

static bool isLoad(const Simulator::Instruction &inst) {
    return inst.readsMem && inst.writesRd && !inst.isNop && inst.isLegal;
}

static bool isArith(const Simulator::Instruction &inst) {
    return inst.doesArithLogic && inst.writesRd && !inst.isNop && inst.isLegal;
}

// Forward values into ID stage operands from EX, MEM, WB
static void forwardToID(Simulator::Instruction &idInst,
                        const Simulator::Instruction &exInst,
                        const Simulator::Instruction &memInst,
                        const Simulator::Instruction &wbInst) {
    // rs1
    if (idInst.readsRs1 && idInst.rs1 != 0) {
        // Priority: EX (ALU) -> MEM -> WB
        if (isArith(exInst) && exInst.rd == idInst.rs1) {
            uint64_t oldVal = idInst.op1Val;
            idInst.op1Val = exInst.arithResult;
            std::cout << "[Forward to ID] Arith to " << regNames[idInst.rs1]
                      << " with " << std::hex << idInst.op1Val
                      << " updating existing value of " << oldVal << std::dec
                      << "\n";
        } else if (memInst.writesRd && memInst.rd == idInst.rs1) {
            if (isLoad(memInst)) {
                uint64_t oldVal = idInst.op1Val;
                idInst.op1Val = memInst.memResult;
                std::cout << "[Forward to ID] Load to " << regNames[idInst.rs1]
                          << " with " << std::hex << idInst.op1Val
                          << " updating existing value of " << oldVal
                          << std::dec << "\n";
            } else if (memInst.doesArithLogic) {
                uint64_t oldVal = idInst.op1Val;
                idInst.op1Val = memInst.arithResult;
                std::cout << "[Forward to ID] Arith to " << regNames[idInst.rs1]
                          << " with " << std::hex << idInst.op1Val
                          << " updating existing value of " << oldVal
                          << std::dec << "\n";
            }
        } else if (wbInst.writesRd && wbInst.rd == idInst.rs1) {
            if (isLoad(wbInst)) {
                uint64_t oldVal = idInst.op1Val;
                idInst.op1Val = wbInst.memResult;
                std::cout << "[Forward to ID] Load to " << regNames[idInst.rs1]
                          << " with " << std::hex << idInst.op1Val
                          << " updating existing value of " << oldVal
                          << std::dec << "\n";
            } else if (wbInst.doesArithLogic) {
                uint64_t oldVal = idInst.op1Val;
                idInst.op1Val = wbInst.arithResult;
                std::cout << "[Forward to ID] Arith to " << regNames[idInst.rs1]
                          << " with " << std::hex << idInst.op1Val
                          << " updating existing value of " << oldVal
                          << std::dec << "\n";
            }
        }
    }

    // rs2
    if (idInst.readsRs2 && idInst.rs2 != 0) {
        if (isArith(exInst) && exInst.rd == idInst.rs2) {
            uint64_t oldVal = idInst.op2Val;
            idInst.op2Val = exInst.arithResult;
            std::cout << "[Forward to ID] Arith to " << regNames[idInst.rs2]
                      << " with " << std::hex << idInst.op2Val
                      << " updating existing value of " << oldVal << std::dec
                      << "\n";
        } else if (memInst.writesRd && memInst.rd == idInst.rs2) {
            if (isLoad(memInst)) {
                uint64_t oldVal = idInst.op2Val;
                idInst.op2Val = memInst.memResult;
                std::cout << "[Forward to ID] Load to " << regNames[idInst.rs2]
                          << " with " << std::hex << idInst.op2Val
                          << " updating existing value of " << oldVal
                          << std::dec << "\n";
            } else if (memInst.doesArithLogic) {
                uint64_t oldVal = idInst.op2Val;
                idInst.op2Val = memInst.arithResult;
                std::cout << "[Forward to ID] Arith to " << regNames[idInst.rs2]
                          << " with " << std::hex << idInst.op2Val
                          << " updating existing value of " << oldVal
                          << std::dec << "\n";
            }
        } else if (wbInst.writesRd && wbInst.rd == idInst.rs2) {
            if (isLoad(wbInst)) {
                uint64_t oldVal = idInst.op2Val;
                idInst.op2Val = wbInst.memResult;
                std::cout << "[Forward to ID] Load to " << regNames[idInst.rs2]
                          << " with " << std::hex << idInst.op2Val
                          << " updating existing value of " << oldVal
                          << std::dec << "\n";
            } else if (wbInst.doesArithLogic) {
                uint64_t oldVal = idInst.op2Val;
                idInst.op2Val = wbInst.arithResult;
                std::cout << "[Forward to ID] Arith to " << regNames[idInst.rs2]
                          << " with " << std::hex << idInst.op2Val
                          << " updating existing value of " << oldVal
                          << std::dec << "\n";
            }
        }
    }
}

Simulator::Instruction Simulator::simID(Simulator::Instruction inst,
                                        const Simulator::Instruction &exInst,
                                        const Simulator::Instruction &memInst,
                                        const Simulator::Instruction &wbInst) {
    if (inst.isNop || inst.isHalt)
        return inst;

    inst = simDecode(inst);

    // illegal instruction handling: mark now, handled in cycle.cpp later
    if (!inst.isLegal) {
        return inst;
    }

    inst = simOperandCollection(inst, regData);

    // Forwarding to ID stage
    forwardToID(inst, exInst, memInst, wbInst);

    inst = simNextPCResolution(inst);

    // inst.status = NORMAL;
    return inst; // TODO implement ID
}

Simulator::Instruction Simulator::simEX(Simulator::Instruction inst) {
    if (inst.isNop || inst.isHalt || !inst.isLegal)
        return inst;

    // arithmetic instructions
    if (inst.doesArithLogic) {
        inst = simArithLogic(inst);
    }

    // load/store address generation
    if (inst.readsMem || inst.writesMem) {
        inst = simAddrGen(inst);
    }

    // inst.status = NORMAL;
    return inst; // TODO implement EX
}

Simulator::Instruction Simulator::simMEM(Simulator::Instruction inst) {
    if (inst.isNop || inst.isHalt || !inst.isLegal)
        return inst;

    // actual memory access
    if (inst.readsMem || inst.writesMem) {
        inst = simMemAccess(inst, memory);
    }

    // inst.status = NORMAL;
    return inst;
} // TODO implement MEM

Simulator::Instruction Simulator::simWB(Simulator::Instruction inst) {
    if (inst.isNop || inst.isHalt || !inst.isLegal || inst.memException) {
        return inst;
    }

    if (inst.writesRd) {
        inst = simCommit(inst, regData);
    }

    // inst.status = NORMAL;
    return inst; // TODO implement WB
}

// Simulate the whole instruction using functions above
Simulator::Instruction Simulator::simInstruction(uint64_t PC) {
    // Implementation moved from .cpp to .h for illustration
    Instruction inst = simFetch(PC, memory);
    inst = simDecode(inst);
    inst.instructionID = din++;
    if (!inst.isLegal || inst.isHalt)
        return inst;
    inst = simOperandCollection(inst, regData);
    inst = simNextPCResolution(inst);
    if (inst.doesArithLogic)
        inst = simArithLogic(inst);
    if (inst.readsMem || inst.writesMem) {
        inst = simAddrGen(inst);
        inst = simMemAccess(inst, memory);
    }
    if (inst.writesRd)
        inst = simCommit(inst, regData);
    PC = inst.nextPC;
    return inst;
}