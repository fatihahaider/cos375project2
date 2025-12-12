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

    // We will handle Arith->Branch by forwarding + recalculating nextPC in ID.
    // So we do NOT need to stall.
    return false;
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

// Forward values into ID stage operands from EX, MEM, WB
static void forwardToID(Simulator::Instruction &idInst,
                        const Simulator::Instruction &exInst,
                        const Simulator::Instruction &memInst,
                        const Simulator::Instruction &wbInst) {
    // rs1
    if (idInst.readsRs1 && idInst.rs1 != 0) {
        // Priority: EX (ALU) -> MEM -> WB
        if (isArith(exInst) && exInst.rd == idInst.rs1) {
            idInst.op1Val = exInst.arithResult;
        } else if (memInst.writesRd && memInst.rd == idInst.rs1) {
            if (isLoad(memInst)) {
                idInst.op1Val = memInst.memResult;
            } else if (memInst.doesArithLogic) {
                idInst.op1Val = memInst.arithResult;
            }
        } else if (wbInst.writesRd && wbInst.rd == idInst.rs1) {
            if (isLoad(wbInst)) {
                idInst.op1Val = wbInst.memResult;
            } else if (wbInst.doesArithLogic) {
                idInst.op1Val = wbInst.arithResult;
            }
        }
    }

    // rs2
    if (idInst.readsRs2 && idInst.rs2 != 0) {
        if (isArith(exInst) && exInst.rd == idInst.rs2) {
            idInst.op2Val = exInst.arithResult;
        } else if (memInst.writesRd && memInst.rd == idInst.rs2) {
            if (isLoad(memInst)) {
                idInst.op2Val = memInst.memResult;
            } else if (memInst.doesArithLogic) {
                idInst.op2Val = memInst.arithResult;
            }
        } else if (wbInst.writesRd && wbInst.rd == idInst.rs2) {
            if (isLoad(wbInst)) {
                idInst.op2Val = wbInst.memResult;
            } else if (wbInst.doesArithLogic) {
                idInst.op2Val = wbInst.arithResult;
            }
        }
    }
}

// Helper Function to Forward into EX stage 3 Different Ways
static void forwardToEX(Simulator::Instruction &exInput,
                        const Simulator::Instruction &exInst,
                        const Simulator::Instruction &memInst,
                        const Simulator::Instruction &wbInst) {

    // register 1
    if (exInput.readsRs1 && exInput.rs1 != 0) {

        // EX/MEM -> EX
        if (exInst.writesRd && exInst.rd == exInput.rs1 && !isLoad(exInst)) {
            exInput.op1Val = exInst.arithResult;
        }

        // MEM/WB -> EX
        else if (memInst.writesRd && memInst.rd == exInput.rs1) {
            if (isLoad(memInst))
                exInput.op1Val = memInst.memResult;
            else if (memInst.doesArithLogic)
                exInput.op1Val = memInst.arithResult;
        }

        // WB -> EX
        else if (wbInst.writesRd && wbInst.rd == exInput.rs1) {
            if (isLoad(wbInst))
                exInput.op1Val = wbInst.memResult;
            else if (wbInst.doesArithLogic)
                exInput.op1Val = wbInst.arithResult;
        }
    }

    // register 2
    if (exInput.readsRs2 && exInput.rs2 != 0) {

        // EX/MEM -> EX
        if (exInst.writesRd && exInst.rd == exInput.rs2 && !isLoad(exInst)) {
            exInput.op2Val = exInst.arithResult;
        }

        // MEM/WB -> EX
        else if (memInst.writesRd && memInst.rd == exInput.rs2) {
            if (isLoad(memInst))
                exInput.op2Val = memInst.memResult;
            else if (memInst.doesArithLogic)
                exInput.op2Val = memInst.arithResult;
        }

        // WB -> EX
        else if (wbInst.writesRd && wbInst.rd == exInput.rs2) {
            if (isLoad(wbInst))
                exInput.op2Val = wbInst.memResult;
            else if (wbInst.doesArithLogic)
                exInput.op2Val = wbInst.arithResult;
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
        memInst.op2Val = wbInst.memResult;
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
        }

        if (hazardStall > 0) {
            // Waiting on data hazard. Freeze ID and below.
            pipelineInfo.exInst = nop(BUBBLE);
            pipelineInfo.idInst = prev.idInst;
            pipelineInfo.ifInst = prev.ifInst;
        } else {
            // Normal Pipeline: ID -> EX
            pipelineInfo.exInst = prev.idInst;

            // IF -> ID Advancement Logic
            if (isBranch(prev.ifInst) && prev.ifInst.PC != PC) {
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

            // If ID is a branch then set IF to speculative
            if (isBranch(pipelineInfo.idInst) &&
                pipelineInfo.idInst.status == NORMAL) {
                pipelineInfo.ifInst.status = SPECULATIVE;
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
        forwardToEX(pipelineInfo.exInst, pipelineInfo.exInst,
                    pipelineInfo.memInst, pipelineInfo.wbInst);
        pipelineInfo.exInst = simulator->simEX(pipelineInfo.exInst);

        // 2. ID
        if (hazardStall == 0) {
            forwardToID(pipelineInfo.idInst, pipelineInfo.exInst,
                        pipelineInfo.memInst, pipelineInfo.wbInst);
            pipelineInfo.idInst = simulator->simID(pipelineInfo.idInst);

            if (isBranch(pipelineInfo.idInst)) {
                PC = pipelineInfo.idInst.nextPC;
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