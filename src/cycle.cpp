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

PipeState pipeState = {0};

// stall & stats
static int loadBranchStallCycles = 0; // remaining cycles of a load->branch stall
static uint64_t loadStallCount = 0; // total load-related stall cycles

// cache miss timing
static int iMissCyclesLeft = 0; // remaining extra cycles for current I-cache miss
static int dMissCyclesLeft = 0; // remaining extra cycles for current D-cache miss

static const uint64_t EXCEPTION_HANDLER_ADDR = 0x8000;

static bool exceptionPending = false; // "next cycle, start at handler"

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

Status runCycles(uint64_t cycles) {
    uint64_t count = 0;
    auto status = SUCCESS;
    
    while (cycles == 0 || count < cycles) {
        std::cout << cycleCount << "\n";

        pipeState.cycle = cycleCount;
        count++;
        cycleCount++;
        bool exceptionFromID = false;
        bool exceptionFromMEM = false;
        bool skipIF = false;
        bool dMissActive = (dMissCyclesLeft > 0);

        // If we raised an exception last cycle, redirect PC now
        if (exceptionPending) {
            PC = EXCEPTION_HANDLER_ADDR;
            exceptionPending = false;

            // Start fresh in IF/ID; older instructions in EX/MEM/WB will drain
            pipelineInfo.ifInst = nop(IDLE);
            pipelineInfo.idInst = nop(IDLE);
        }

        // Snapshot previous pipeline state at start of cycle
        PipelineInfo prev = pipelineInfo;

        // === 1. WB stage ===
        if (dMissActive) {
            // The memory instruction hasn't finished its miss yet,dMissActive
            // so nothing can commit this cycle.
            pipelineInfo.wbInst = nop(BUBBLE);

        } else {
            pipelineInfo.wbInst = simulator->simWB(prev.memInst);

            if (!pipelineInfo.wbInst.isNop) {
                pipelineInfo.wbInst.status = NORMAL;
            }
        }

        // WB: halt check (HALT only when it reaches WB)
        if (pipelineInfo.wbInst.isHalt) {
            status = HALT;
            break;
        }

        // === 2. Hazard detection (EX producer, ID consumer) ===
        bool stallIF = false;
        bool stallID = false;
        bool bubbleEX = false;

        bool loadUseHazard = hasLoadUseHazard(prev.exInst, prev.idInst);
        bool arithBranchHazard = hasArithBranchHazard(prev.exInst, prev.idInst);
        bool loadBranchHazard = hasLoadBranchHazard(prev.exInst, prev.idInst);

        // Special case: load->store (using rd only as rs2) should NOT stall
        if (loadUseHazard && isStore(prev.idInst) && prev.idInst.readsRs2 &&
            !(prev.idInst.readsRs1 && prev.idInst.rs1 == prev.exInst.rd) &&
            prev.idInst.rs2 == prev.exInst.rd) {
            loadUseHazard = false;
        }

        if (loadBranchStallCycles > 0) {
            // already in the middle of a 2-cycle load->branch stall
            stallIF = stallID = true;
            bubbleEX = true;
            loadBranchStallCycles--;

        } else if (loadBranchHazard) {
            // start a 2-cycle load->branch stall
            stallIF = stallID = true;
            bubbleEX = true;
            loadBranchStallCycles = 1; // this cycle + next
            loadStallCount++;

        } else if (loadUseHazard) {
            // 1-cycle load-use stall
            stallIF = stallID = true;
            bubbleEX = true;
            loadStallCount++;

        } else if (arithBranchHazard) {
            // 1-cycle arithmetic->branch stall
            stallIF = stallID = true;
            bubbleEX = true;
        }

        // === 2.5 D-cache miss: stall IF/ID/EX while miss in MEM ===
        if (dMissActive) {
            // Current MEM instruction is waiting on a D-cache miss.
            // Younger instructions must stall; EX should hold its current
            // instruction (so do NOT bubble it because of the miss).
            stallIF = true;
            stallID = true;
        }

        // === 3. MEM stage (with D-cache timing + load->store forwarding) ===
        if (dMissCyclesLeft > 0) {
            // In the middle of a D-cache miss: keep the same instruction in MEM
            pipelineInfo.memInst = prev.memInst;
            // pipelineInfo.memInst.status = NORMAL;
            dMissCyclesLeft--;

        } else {
            // Potentially new memory access coming from EX
            Simulator::Instruction memInput = prev.exInst;

            // forward from WB (load result) into store data if needed
            forwardLoadToStore(memInput, pipelineInfo.wbInst);

            if (memInput.readsMem || memInput.writesMem) {
                bool hit = dCache->access(memInput.memAddress,
                                          memInput.readsMem ? CACHE_READ
                                                            : CACHE_WRITE);

                if (hit) {
                    // Hit: MEM completes this cycle
                    pipelineInfo.memInst = simulator->simMEM(memInput);
                    if (!pipelineInfo.memInst.isNop) {
                    pipelineInfo.memInst.status = NORMAL;
                    }

                } else {
                    // Miss: this instruction enters MEM now, and then stays
                    // for config.missLatency extra cycles.
                    pipelineInfo.memInst = simulator->simMEM(memInput);
                    if (!pipelineInfo.memInst.isNop) {
                    pipelineInfo.memInst.status = NORMAL;
                    }
                    dMissCyclesLeft =
                        static_cast<int>(dCache->config.missLatency);
                }
            } else {
                // Non-memory instruction: just pass through MEM
                pipelineInfo.memInst = simulator->simMEM(memInput);
                if (!pipelineInfo.memInst.isNop) {
                pipelineInfo.memInst.status = NORMAL;
                }
            }
        }

        // After MEM: detect memory exception (bad address)
        if (pipelineInfo.memInst.memException) {
            exceptionFromMEM = true;
        }

        // === 4. EX stage ===
        if (dMissActive) {
            // While a D-miss is active, EX must stall (hold its current
            // instruction)
            pipelineInfo.exInst = prev.exInst;
            // pipelineInfo.exInst.status = NORMAL;

        } else if (bubbleEX) {
            pipelineInfo.exInst = nop(BUBBLE);

        } else {

            Simulator::Instruction exInput = prev.idInst;

            // forwarding into ex stage
            forwardToEX(exInput, prev.exInst, prev.memInst,
                        pipelineInfo.wbInst);
            pipelineInfo.exInst = simulator->simEX(exInput);
            if (!pipelineInfo.exInst.isNop) {
            pipelineInfo.exInst.status = NORMAL;
            }
        }

        // === 5. ID stage ===
        if (stallID) {
            // hold previous instruction in ID
            pipelineInfo.idInst = prev.idInst;
            // pipelineInfo.idInst.status = NORMAL;
        } else {
            pipelineInfo.idInst = simulator->simID(prev.ifInst);
            if (!pipelineInfo.idInst.isNop) {
            pipelineInfo.idInst.status = NORMAL;
            }
        }

        // Forwarding into ID operands (for branches + dependent ALU ops)
        forwardToID(pipelineInfo.idInst, pipelineInfo.exInst,
                    pipelineInfo.memInst, pipelineInfo.wbInst);

        // Crucial Fix: Re-calculate nextPC now that op1Val/op2Val might have
        // changed due to forwarding
        pipelineInfo.idInst =
            simulator->simNextPCResolution(pipelineInfo.idInst);

        bool branchInID = isBranch(pipelineInfo.idInst);
        bool branchTaken = false;

        if (branchInID) {
            branchTaken =
                (pipelineInfo.idInst.nextPC != pipelineInfo.idInst.PC + 4);
        }

        if (branchInID && branchTaken && !stallID && !stallIF &&
            !exceptionFromID && !exceptionFromMEM) {

            // Squash mispredicted instruction in IF
            pipelineInfo.ifInst = nop(SQUASHED);

            // Redirect PC to branch target
            PC = pipelineInfo.idInst.nextPC;

            // Skip IF fetch logic below
            skipIF = true;
        }

        // Detect illegal instruction exception in ID
        if (!pipelineInfo.idInst.isNop && !pipelineInfo.idInst.isHalt &&
            !pipelineInfo.idInst.isLegal) {
            exceptionFromID = true;
        }

        // === 6. IF stage with proper stall and fill behavior ===
        if (!skipIF) {

            // If we must stall IF due to hazard or D‑cache miss, hold instruction
            if (stallIF || prev.idInst.status == BUBBLE || prev.idInst.status == IDLE) {
                pipelineInfo.ifInst = prev.ifInst;
            }
            // If there’s an ongoing I‑cache miss, hold PC and IF
            else if (iMissCyclesLeft > 0) {
                pipelineInfo.ifInst = prev.ifInst;
                iMissCyclesLeft--;
            }
            // Otherwise, fetch a new instruction
            else {
                bool hit = iCache->access(PC, CACHE_READ);
                uint64_t fetchPC = PC;

                if (hit) {
                    // Advance PC only when ID can accept new instruction
                    PC += 4;
                    pipelineInfo.ifInst = simulator->simIF(fetchPC);
                    pipelineInfo.ifInst.status = NORMAL;
                } else {
                    // I‑cache miss — stall IF until miss completes
                    pipelineInfo.ifInst = prev.ifInst;
                    iMissCyclesLeft = static_cast<int>(iCache->config.missLatency);
                }

                // If this is a branch instruction, mark speculative
                if ((pipelineInfo.ifInst.instruction & 0x7F) == 0x63 &&
                    pipelineInfo.ifInst.status == NORMAL) {
                    pipelineInfo.ifInst.status = SPECULATIVE;
                }
            }

        } else {
            pipelineInfo.ifInst = nop(SQUASHED);
        }

        // === 7. Handle exceptions: squash and arm trap ===
        if (exceptionFromID || exceptionFromMEM) {
            // We will start fetching from handler next cycle
            exceptionPending = true;

            if (exceptionFromMEM) {
                // Excepting instruction is in MEM.
                // Older instructions (WB) may still complete.
                // Younger instructions EX/ID/IF must be squashed.
                pipelineInfo.exInst = nop(BUBBLE);
                pipelineInfo.idInst = nop(BUBBLE);
                pipelineInfo.ifInst = nop(BUBBLE);

                // Also make sure the MEM instruction itself does not propagate
                // as a "normal" instruction. It already has memException=true;
                // simWB will ignore it. You can optionally mark it as NOP for
                // clarity: pipelineInfo.memInst.isNop = true;
            }

            if (exceptionFromID) {
                // Excepting instruction detected in ID.
                // Older ones in EX/MEM/WB drain.
                // The illegal instruction itself must not reach EX/WB.
                // Squash it and any younger IF instruction.
                pipelineInfo.idInst = nop(BUBBLE);
                pipelineInfo.ifInst = nop(BUBBLE);
            }

            // Note: we *don't* touch PC here; PC redirect happens at the
            // top of the *next* cycle when exceptionPending is true.
        }

    // Dump pipe state for the last cycle executed in this call
    pipeState.ifPC = pipelineInfo.ifInst.PC;
    //pipeState.ifStatus = NORMAL; // FIXES PRINTING ISSUE BUT IS NOT THE CLEANEST WAY 
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