#include "cycle.h"

#include <iostream>
#include <memory>
#include <string>

#include "Utilities.h"
#include "cache.h"
#include "simulator.h"

static Simulator *simulator = nullptr;
static Cache *iCache = nullptr;
static Cache *dCache = nullptr;
static std::string output;
static uint64_t cycleCount = 0;

static uint64_t PC = 0; // start PC

// stall & stats
static uint64_t loadStallCount = 0; // total load-related stall cycles

// cache miss timing
static int iMissCyclesLeft =
    0; // remaining extra cycles for current I-cache miss
static int dMissCyclesLeft =
    0; // remaining extra cycles for current D-cache miss

static const uint64_t EXCEPTION_HANDLER_ADDR = 0x8000;

/**TODO: Implement pipeline simulation for the RISCV machine in this file.
 * A basic template is provided below that doesn't account for any hazards.
 */

Simulator::Instruction nop(StageStatus status) {
    Simulator::Instruction nop;
    nop.instruction = 0x00000013;
    nop.isLegal = true;
    nop.isNop = true;
    nop.status = status;
    return nop;
}

static struct PipelineInfo {
    Simulator::Instruction ifInst = nop(IDLE);
    Simulator::Instruction idInst = nop(IDLE);
    Simulator::Instruction exInst = nop(IDLE);
    Simulator::Instruction memInst = nop(IDLE);
    Simulator::Instruction wbInst = nop(IDLE);
} pipelineInfo;

// checks type of instruction
static bool isLoad(const Simulator::Instruction &inst) {
    return inst.readsMem && inst.writesRd && !inst.isNop && inst.isLegal;
}

static bool isStore(const Simulator::Instruction &inst) {
    return inst.writesMem && !inst.isNop && inst.isLegal;
}

static bool isArith(const Simulator::Instruction &inst) {
    return inst.doesArithLogic && inst.writesRd && !inst.isNop && inst.isLegal;
}

static bool isBranch(const Simulator::Instruction &inst) {
    return inst.isLegal && !inst.isNop && !inst.isHalt &&
           inst.opcode == OP_BRANCH;
}

// --- hazard checks ---

// load-use: load in EX, consumer in ID
static bool hasLoadUseHazard(const Simulator::Instruction &producer,
                             const Simulator::Instruction &consumer) {
    if (!isLoad(producer))
        return false;
    if (producer.rd == 0)
        return false;

    bool rs1Haz = consumer.readsRs1 && (consumer.rs1 == producer.rd);
    bool rs2Haz = consumer.readsRs2 && (consumer.rs2 == producer.rd);

    return rs1Haz || rs2Haz;
}

// arithmetic -> branch: ALU producer in EX, branch in ID
static bool hasArithBranchHazard(const Simulator::Instruction &producer,
                                 const Simulator::Instruction &consumer) {
    if (!isArith(producer))
        return false;
    if (!isBranch(consumer))
        return false;
    if (producer.rd == 0)
        return false;

    bool rs1Haz = consumer.readsRs1 && (consumer.rs1 == producer.rd);
    bool rs2Haz = consumer.readsRs2 && (consumer.rs2 == producer.rd);
    return rs1Haz || rs2Haz;
}

// load -> branch: load producer in EX, branch in ID
static bool hasLoadBranchHazard(const Simulator::Instruction &producer,
                                const Simulator::Instruction &consumer) {
    if (!isLoad(producer))
        return false;
    if (!isBranch(consumer))
        return false;
    if (producer.rd == 0)
        return false;

    bool rs1Haz = consumer.readsRs1 && (consumer.rs1 == producer.rd);
    bool rs2Haz = consumer.readsRs2 && (consumer.rs2 == producer.rd);
    return rs1Haz || rs2Haz;
}

// initialize the simulator
Status initSimulator(CacheConfig &iCacheConfig, CacheConfig &dCacheConfig,
                     MemoryStore *mem, const std::string &output_name) {
    output = output_name;
    simulator = new Simulator();
    simulator->setMemory(mem);
    iCache = new Cache(iCacheConfig, I_CACHE);
    dCache = new Cache(dCacheConfig, D_CACHE);
    return SUCCESS;
}

// FORWARD TO ID is in simulator.cpp, since forwarding must take place after
// parsing but before nextPCResolution and operand collection.

// Helper Function to Forward into EX stage 3 Different Ways
static void forwardToEX(Simulator::Instruction &exInput,
                        const Simulator::Instruction &memInst,
                        const Simulator::Instruction &wbInst) {

    // register 1
    if (exInput.readsRs1 && exInput.rs1 != 0) {

        // MEM/WB -> EX
        if (memInst.writesRd && memInst.rd == exInput.rs1) {
            if (isLoad(memInst)) {
                uint64_t oldVal = exInput.op1Val;
                exInput.op1Val = memInst.memResult;
                std::cout << "[Forward to EX] Load to " << regNames[exInput.rs1]
                          << " with " << std::hex << exInput.op1Val
                          << " updating existing value of " << oldVal
                          << std::dec << "\n";
            } else if (memInst.doesArithLogic) {
                uint64_t oldVal = exInput.op1Val;
                exInput.op1Val = memInst.arithResult;
                std::cout << "[Forward to EX] Arith to "
                          << regNames[exInput.rs1] << " with " << std::hex
                          << exInput.op1Val << " updating existing value of "
                          << oldVal << std::dec << "\n";
            }
        }

        // WB -> EX
        else if (wbInst.writesRd && wbInst.rd == exInput.rs1) {
            if (isLoad(wbInst)) {
                uint64_t oldVal = exInput.op1Val;
                exInput.op1Val = wbInst.memResult;
                std::cout << "[Forward to EX] Load to " << regNames[exInput.rs1]
                          << " with " << std::hex << exInput.op1Val
                          << " updating existing value of " << oldVal
                          << std::dec << "\n";
            } else if (wbInst.doesArithLogic) {
                uint64_t oldVal = exInput.op1Val;
                exInput.op1Val = wbInst.arithResult;
                std::cout << "[Forward to EX] Arith to "
                          << regNames[exInput.rs1] << " with " << std::hex
                          << exInput.op1Val << " updating existing value of "
                          << oldVal << std::dec << "\n";
            }
        }
    }

    // register 2
    if (exInput.readsRs2 && exInput.rs2 != 0) {

        // MEM/WB -> EX
        if (memInst.writesRd && memInst.rd == exInput.rs2) {
            if (isLoad(memInst)) {
                uint64_t oldVal = exInput.op2Val;
                exInput.op2Val = memInst.memResult;
                std::cout << "[Forward to EX] Load to " << regNames[exInput.rs2]
                          << " with " << std::hex << exInput.op2Val
                          << " updating existing value of " << oldVal
                          << std::dec << "\n";
            } else if (memInst.doesArithLogic) {
                uint64_t oldVal = exInput.op2Val;
                exInput.op2Val = memInst.arithResult;
                std::cout << "[Forward to EX] Arith to "
                          << regNames[exInput.rs2] << " with " << std::hex
                          << exInput.op2Val << " updating existing value of "
                          << oldVal << std::dec << "\n";
            }
        }

        // WB -> EX
        else if (wbInst.writesRd && wbInst.rd == exInput.rs2) {
            if (isLoad(wbInst)) {
                uint64_t oldVal = exInput.op2Val;
                exInput.op2Val = wbInst.memResult;
                std::cout << "[Forward to EX] Load to " << regNames[exInput.rs2]
                          << " with " << std::hex << exInput.op2Val
                          << " updating existing value of " << oldVal
                          << std::dec << "\n";
            } else if (wbInst.doesArithLogic) {
                uint64_t oldVal = exInput.op2Val;
                exInput.op2Val = wbInst.arithResult;
                std::cout << "[Forward to EX] Arith to "
                          << regNames[exInput.rs2] << " with " << std::hex
                          << exInput.op2Val << " updating existing value of "
                          << oldVal << std::dec << "\n";
            }
        }
    }
}

// Forward loaded value from WB into store's rs2 before MEM
static void forwardLoadToStore(Simulator::Instruction &memInst,
                               const Simulator::Instruction &wbInst) {
    if (!isStore(memInst))
        return;
    if (!isLoad(wbInst))
        return;
    if (!memInst.readsRs2)
        return;
    if (memInst.rs2 == 0)
        return;

    // store uses rs2 as data
    if (memInst.rs2 == wbInst.rd) {
        uint64_t oldVal = memInst.op2Val;
        memInst.op2Val = wbInst.memResult;
        std::cout << "[Forward to MEM] Load to " << regNames[memInst.rs2]
                  << " with " << std::hex << memInst.op2Val
                  << " updating existing value of " << oldVal << std::dec
                  << "\n";
    }
}

// run the simulator for a certain number of cycles
// return SUCCESS if reaching desired cycles.
// return HALT if the simulator halts on 0xfeedfeed

int hazardStall = 0;

Status runCycles(uint64_t cycles) {
    uint64_t count = 0;
    auto status = SUCCESS;
    PipeState pipeState = {0};

    while (cycles == 0 || count < cycles) {
        std::cout << cycleCount << "\n";

        pipeState.cycle = cycleCount;
        count++;
        cycleCount++;

        // Snapshot previous pipeline state at start of cycle
        PipelineInfo prev = pipelineInfo;

        // If we raised an exception last cycle, redirect PC now
        if (prev.memInst.memException || !prev.idInst.isLegal) {
            PC = EXCEPTION_HANDLER_ADDR;

            // Start fresh in IF/ID; older instructions in EX/MEM/WB will drain
            prev.ifInst = nop(BUBBLE);
            prev.idInst = nop(BUBBLE);
        }

        // ----- HAZARD CHECKS ----- //
        bool arithBranchHazard = hasArithBranchHazard(prev.exInst, prev.idInst);
        bool loadUseHazard = hasLoadUseHazard(prev.exInst, prev.idInst);
        bool loadBranchHazard = hasLoadBranchHazard(prev.exInst, prev.idInst);

        if (arithBranchHazard) {
            hazardStall = 1;
        }

        if (loadUseHazard) {
            hazardStall = 1;
            loadStallCount++;
        }

        if (loadBranchHazard) {
            hazardStall = 2;
            loadStallCount++;
        }

        // Special case: load->store (using rd only as rs2) should NOT stall
        if (loadUseHazard && isStore(prev.idInst) && prev.idInst.readsRs2 &&
            !(prev.idInst.readsRs1 && prev.idInst.rs1 == prev.exInst.rd) &&
            prev.idInst.rs2 == prev.exInst.rd) {
            loadUseHazard = false;
        }

        // ----- ADVANCE PIPELINE ----- //
        if (dMissCyclesLeft > 0) {
            // Data Cache Miss: hold MB, bubble WB
            pipelineInfo.memInst = prev.memInst;
            pipelineInfo.wbInst = nop(BUBBLE);
        } else {
            // Normal Pipeline: shift EX/MEM -> MEM/WB
            pipelineInfo.wbInst = prev.memInst;
            pipelineInfo.memInst = prev.exInst;

            if (hazardStall > 0) {
                // Waiting on data hazard. Freeze ID and below.
                pipelineInfo.exInst = nop(BUBBLE);
                pipelineInfo.idInst = prev.idInst;
                pipelineInfo.ifInst = prev.ifInst;
            } else {
                // Normal Pipeline: ID -> EX
                pipelineInfo.exInst = prev.idInst;

                // IF -> ID Advancement Logic
                if (isBranch(prev.idInst) && prev.ifInst.PC != PC - 4) {
                    // Branch misprediction: squash ID
                    pipelineInfo.idInst = nop(SQUASHED);
                } else if (iMissCyclesLeft > 0) {
                    // Instruction Cache Miss: hold IF, bubble ID
                    pipelineInfo.idInst = nop(BUBBLE);
                } else {
                    // Normal Pipeline: IF -> ID
                    pipelineInfo.idInst = prev.ifInst;
                }

                // Load IF if not in a miss
                if (iMissCyclesLeft == 0) {
                    bool hit = iCache->access(PC, CACHE_READ);
                    pipelineInfo.ifInst =
                        simulator->simIF(PC);    // Fetch from saved PC
                    pipelineInfo.ifInst.PC = PC; // Preserve PC
                    pipelineInfo.ifInst.status = NORMAL;

                    if (!hit) {
                        iMissCyclesLeft =
                            static_cast<int>(iCache->config.missLatency) + 1;
                    }

                    // Advance PC
                    PC += 4;
                }
            }
        }

        // --- SIMULATE STAGES --- //
        // HALT check
        if (pipelineInfo.wbInst.isHalt) {
            status = HALT;
            break;
        }

        // 5. WB
        pipelineInfo.wbInst = simulator->simWB(pipelineInfo.wbInst);

        // 4. MEM
        if (dMissCyclesLeft > 0) {
            // In the middle of a D-cache miss: keep the same instruction in MEM
            pipelineInfo.memInst = prev.memInst;
        } else {
            // forward from WB (load result) into store data if needed
            forwardLoadToStore(pipelineInfo.memInst, pipelineInfo.wbInst);

            if (pipelineInfo.memInst.readsMem ||
                pipelineInfo.memInst.writesMem) {
                bool hit = dCache->access(
                    pipelineInfo.memInst.memAddress,
                    pipelineInfo.memInst.readsMem ? CACHE_READ : CACHE_WRITE);

                if (!hit) {
                    dMissCyclesLeft =
                        static_cast<int>(dCache->config.missLatency) + 1;
                }
            } // Non-memory instruction: just pass through MEM

            pipelineInfo.memInst = simulator->simMEM(pipelineInfo.memInst);
        }

        // 3. EX
        forwardToEX(pipelineInfo.exInst, pipelineInfo.memInst,
                    pipelineInfo.wbInst);
        pipelineInfo.exInst = simulator->simEX(pipelineInfo.exInst);

        // 2. ID
        if (hazardStall == 0) {
            if (pipelineInfo.idInst.status == SPECULATIVE)
                pipelineInfo.idInst.status =
                    NORMAL; // spec instructions that made it this far are real
                            // now

            pipelineInfo.idInst =
                simulator->simID(pipelineInfo.idInst, pipelineInfo.exInst,
                                 pipelineInfo.memInst, pipelineInfo.wbInst);

            if (isBranch(pipelineInfo.idInst)) {
                // Debug message: idInst nextPC and ifInst PC, and current PC
                std::cout << "[Branch Check] ID nextPC: " << std::hex
                          << pipelineInfo.idInst.nextPC
                          << ", IF PC: " << pipelineInfo.ifInst.PC
                          << ", Current PC: " << (PC - 4) << std::dec << "\n";

                pipelineInfo.ifInst.status = SPECULATIVE;

                if (pipelineInfo.idInst.nextPC != pipelineInfo.ifInst.PC) {
                    PC = pipelineInfo.idInst.nextPC;
                } else {
                    // Branch not taken; continue as normal
                }
            }
        }

        // 1. IF (logic handled above)

        // Decrement all miss/stall counters.
        if (hazardStall > 0)
            hazardStall--;

        if (iMissCyclesLeft > 0)
            iMissCyclesLeft--;

        if (dMissCyclesLeft > 0)
            dMissCyclesLeft--;

        // Dump pipe state for the last cycle executed in this call
        pipeState.ifPC = pipelineInfo.ifInst.PC;
        pipeState.ifStatus = pipelineInfo.ifInst.status;
        pipeState.idInstr = pipelineInfo.idInst.instruction;
        pipeState.idStatus = pipelineInfo.idInst.status;
        pipeState.exInstr = pipelineInfo.exInst.instruction;
        pipeState.exStatus = pipelineInfo.exInst.status;
        pipeState.memInstr = pipelineInfo.memInst.instruction;
        pipeState.memStatus = pipelineInfo.memInst.status;
        pipeState.wbInstr = pipelineInfo.wbInst.instruction;
        pipeState.wbStatus = pipelineInfo.wbInst.status;
    }

    dumpPipeState(pipeState, output);
    return status;
}

// run till halt (call runCycles() with cycles == 1 each time) until
// status tells you to HALT or ERROR out
Status runTillHalt() {
    Status status;
    while (true) {
        status = static_cast<Status>(runCycles(1));

        if (status == HALT)
            break;
    }
    return status;
}

// dump the state of the simulator
Status finalizeSimulator() {
    simulator->dumpRegMem(output);

    uint64_t icHits = iCache ? iCache->getHits() : 0;
    uint64_t icMisses = iCache ? iCache->getMisses() : 0;
    uint64_t dcHits = dCache ? dCache->getHits() : 0;
    uint64_t dcMisses = dCache ? dCache->getMisses() : 0;

    SimulationStats stats{
        simulator->getDin(), // dynamic instructions
        cycleCount,          // total cycles
        icHits,
        icMisses,
        dcHits,
        dcMisses,
        loadStallCount // load stalls (events not cycles)
    };

    dumpSimStats(stats, output);
    return SUCCESS;
}