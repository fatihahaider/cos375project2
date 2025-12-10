#pragma once
#include <inttypes.h>
#include <iostream>
#include <vector>
#include "Utilities.h"

struct CacheConfig {
    // Cache size in bytes.
    uint64_t cacheSize;
    // Cache block size in bytes.
    uint64_t blockSize;
    // Type of cache: set associativity 
    uint64_t ways;
    // Additional miss latency in cycles.
    uint64_t missLatency;
    // debug: Overload << operator to allow easy printing of CacheConfig
    friend std::ostream& operator<<(std::ostream& os, const CacheConfig& config) {
        os << "CacheConfig { " << config.cacheSize << ", " << config.blockSize << ", "
           << config.ways << ", " << config.missLatency << " }";
        return os;
    }
};

enum CacheDataType { I_CACHE = false, D_CACHE = true };
enum CacheOperation { CACHE_READ = false, CACHE_WRITE = true };

class Cache {
private:
    struct Line {
        bool valid = false;
        uint64_t tag = 0;
        uint64_t lruCounter = 0;  // for true LRU
    };

    uint64_t hits = 0; 
    misses = 0;
    CacheDataType type;

    // LRU bookkeeping
    uint64_t numSets = 0;
    uint64_t blockOffsetBits = 0;
    uint64_t indexBits = 0;
    uint64_t globalCounter = 0; // global “time” to implement LRU

    std::vector<std::vector<Line>> sets; // [set][way]

     // helper to compute log2 when argument is power of 2
    uint64_t log2Pow2(uint64_t x) {
        uint64_t b = 0;
        while ((1ULL << b) < x) ++b;
        return b;
    }

public:
    CacheConfig config;
    // Constructor to initialize the cache parameters
    Cache(CacheConfig configParam, CacheDataType cacheType);

    /** Access methods for reading/writing
     * @return true for hit and false for miss
     * @param
     *  address: memory address
     *  readWrite: true for write operation and false for read operation
     */
    bool access(uint64_t address, CacheOperation readWrite);

    // debug: dump information as you needed
    Status dump(const std::string& base_output_name);

    uint64_t getHits()   { return hits; }
    uint64_t getMisses() { return misses; }
};

