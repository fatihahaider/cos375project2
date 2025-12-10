// Sample cache implementation with random hit return
// TODO: Modify this file to model an LRU cache as in the project description

#include "cache.h"
#include <random>

using namespace std;

// Random generator for cache hit/miss simulation (RANDOM HIT RETURN IMPLEMENTATION)
/* static std::mt19937 generator(42);  Fixed seed for deterministic results
std::uniform_real_distribution<double> distribution(0.0, 1.0); */

// Constructor definition
Cache::Cache(CacheConfig configParam, CacheDataType cacheType) : hits(0), misses(0), 
type(cacheType), numSets(0), blockOffsetBits(0), indexBits(0), 
sets(), 
globalCounter(0),
config(configParam) {
    // Here you can initialize other cache-specific attributes
    // For instance, if you had cache tables or other structures, initialize them here

    // number of sets = total_size / (block_size * ways)
    numSets = config.cacheSize / (config.blockSize * config.ways);

    // compute bits for block offset and index (assume powers of 2)
    blockOffsetBits = log2Pow2(config.blockSize);
    indexBits       = log2Pow2(numSets);

    // allocate sets and initialize lines as invalid
    sets.resize(numSets, std::vector<Line>(config.ways));
    for (uint64_t s = 0; s < numSets; ++s) {
        for (uint64_t w = 0; w < config.ways; ++w) {
            sets[s][w].valid = false;
            sets[s][w].tag = 0;
            sets[s][w].lruCounter = 0;
        }
    }
}

// Access method definition
bool Cache::access(uint64_t address, CacheOperation readWrite) {
    (void)readWrite; // timing is the same for read/write in this project

    globalCounter++;

    // derive index and tag from address
    uint64_t blockAddr = address >> blockOffsetBits;        // drop block offset
    uint64_t indexMask = numSets - 1;                       // numSets is power of 2
    uint64_t setIndex  = blockAddr & indexMask;
    uint64_t tag       = blockAddr >> indexBits;

    auto &set = sets[setIndex];

    // 1) Look for a hit
    int hitWay = -1;
    for (uint64_t w = 0; w < config.ways; ++w) {
        if (set[w].valid && set[w].tag == tag) {
            hitWay = static_cast<int>(w);
            break;
        }
    }

    if (hitWay != -1) {
        // HIT: update LRU info
        hits++;
        set[hitWay].lruCounter = globalCounter;
        return true;
    }

    // 2) Miss: find an invalid line first
    misses++;
    int victimWay = -1;
    for (uint64_t w = 0; w < config.ways; ++w) {
        if (!set[w].valid) {
            victimWay = static_cast<int>(w);
            break;
        }
    }

    // 3) If no invalid line, choose the true LRU (smallest lruCounter)
    if (victimWay == -1) {
        uint64_t minCounter = set[0].lruCounter;
        victimWay = 0;
        for (uint64_t w = 1; w < config.ways; ++w) {
            if (set[w].lruCounter < minCounter) {
                minCounter = set[w].lruCounter;
                victimWay = static_cast<int>(w);
            }
        }
    }

    // fill the victim line
    set[victimWay].valid      = true;
    set[victimWay].tag        = tag;
    set[victimWay].lruCounter = globalCounter;

    return false;
}

// debug: dump information as you needed, here are some examples
Status Cache::dump(const std::string& base_output_name) {
    ofstream cache_out(base_output_name + "_cache_state.out");
    if (cache_out) {
        cache_out << "---------------------" << endl;
        cache_out << "Begin Register Values" << endl;
        cache_out << "---------------------" << endl;
        cache_out << "Cache Configuration:" << std::endl;
        cache_out << "Size: " << config.cacheSize << " bytes" << std::endl;
        cache_out << "Block Size: " << config.blockSize << " bytes" << std::endl;
        cache_out << "Ways: " << (config.ways == 1) << std::endl;
        cache_out << "Miss Latency: " << config.missLatency << " cycles" << std::endl;
        cache_out << "---------------------" << endl;
        cache_out << "End Register Values" << endl;
        cache_out << "---------------------" << endl;
        return SUCCESS;
    } else {
        cerr << LOG_ERROR << "Could not create cache state dump file" << endl;
        return ERROR;
    }
}
