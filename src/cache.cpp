// Sample cache implementation with random hit return
// TODO: Modify this file to model an LRU cache as in the project description

#include "cache.h"
#include <random>

using namespace std;

// Random generator for cache hit/miss simulation (RANDOM HIT RETURN
// IMPLEMENTATION)
/* static std::mt19937 generator(42);  Fixed seed for deterministic results
std::uniform_real_distribution<double> distribution(0.0, 1.0); */

Cache::Cache(CacheConfig configParam, CacheDataType cacheType)
    : hits(0), misses(0), type(cacheType), globalCounter(0),
      config(configParam) {
    // Invariant: cacheSize is divisible by blockSize * ways
    numSets = config.cacheSize / (config.blockSize * config.ways);

    // compute bits for block offset and index (assume powers of 2)
    blockOffsetBits = log2Pow2(config.blockSize);
    indexBits = log2Pow2(numSets);

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
    uint64_t blockAddr = address >> blockOffsetBits; // drop block offset
    uint64_t indexMask = numSets - 1;                // numSets is power of 2
    uint64_t setIndex = blockAddr & indexMask;
    uint64_t tag = blockAddr >> indexBits;

    auto &set = sets[setIndex];

    // Check for hit in any one of the ways
    int hitWay = -1;
    for (uint64_t w = 0; w < config.ways; w++) {
        if (set[w].valid && set[w].tag == tag) {
            hitWay = static_cast<int>(w);
            break;
        }
    }

    // Cache Hit
    if (hitWay != -1) {
        hits++;
        set[hitWay].lruCounter = globalCounter; // LRU timing update
        return true;
    }

    // ---------------------
    //      Cache Miss
    // ---------------------

    misses++;
    int victimWay = -1;

    // Check for invalid way
    for (uint64_t w = 0; w < config.ways; ++w) {
        if (!set[w].valid) {
            victimWay = static_cast<int>(w);
            break;
        }
    }

    // If no invalid way, evict the LRU.
    if (victimWay == -1) {
        victimWay = 0;
        uint64_t minCounter = set[0].lruCounter;

        for (uint64_t w = 1; w < config.ways; ++w) {
            if (set[w].lruCounter < minCounter) {
                minCounter = set[w].lruCounter;
                victimWay = static_cast<int>(w);
            }
        }
    }

    // fill the victim line
    set[victimWay].valid = true;
    set[victimWay].tag = tag;
    set[victimWay].lruCounter = globalCounter;

    return false;
}

// debug: dump information as you needed, here are some examples
Status Cache::dump(const std::string &base_output_name) {
    ofstream cache_out(base_output_name + "_cache_state.out");
    if (cache_out) {
        cache_out << "---------------------" << endl;
        cache_out << "Begin Register Values" << endl;
        cache_out << "---------------------" << endl;
        cache_out << "Cache Configuration:" << std::endl;
        cache_out << "Size: " << config.cacheSize << " bytes" << std::endl;
        cache_out << "Block Size: " << config.blockSize << " bytes"
                  << std::endl;
        cache_out << "Ways: " << (config.ways == 1) << std::endl;
        cache_out << "Miss Latency: " << config.missLatency << " cycles"
                  << std::endl;
        cache_out << "---------------------" << endl;
        cache_out << "End Register Values" << endl;
        cache_out << "---------------------" << endl;
        return SUCCESS;
    } else {
        cerr << LOG_ERROR << "Could not create cache state dump file" << endl;
        return ERROR;
    }
}
