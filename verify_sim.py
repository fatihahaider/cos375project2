#!/usr/bin/env python3
import sys
import os
import subprocess

def verify_sim(binary_file, cache_config):
    # Determine base filename logic as in sim_cycle.cpp
    # auto baseFilename = getBaseFilename(argv[1]) + "_cycle";
    # If argv[1] is "test/fib.bin", baseFilename is "test/fib_cycle"
    # Note: getBaseFilename removes extension.
    
    base_name = os.path.splitext(binary_file)[0]
    base_output_prefix = base_name + "_cycle"
    
    # Run sim_cycle
    cmd = ["./sim_cycle", binary_file, cache_config]
    print(f"Running: {' '.join(cmd)}")
    result = subprocess.run(cmd, capture_output=True, text=True)
    
    if result.returncode != 0:
        print("Simulation failed!")
        print(result.stderr)
        return False

    # Define output suffixes and their corresponding reference suffixes
    suffixes = [
        "_pipe_state.out",
        "_reg_state.out",
        "_mem_state.out",
        "_sim_stats.out",
    ]
    
    all_passed = True
    
    for suffix in suffixes:
        out_file = base_output_prefix + suffix
        ref_file = base_output_prefix + suffix.replace(".out", ".ref")
        
        if not os.path.exists(ref_file):
            print(f"[WARNING] Reference file {ref_file} not found. Skipping.")
            continue
            
        if not os.path.exists(out_file):
            print(f"[FAIL] Output file {out_file} was not generated.")
            all_passed = False
            continue
            
        # Compare files
        # Using readlines to ignore line ending differences if any
        try:
            with open(out_file, 'r') as f1, open(ref_file, 'r') as f2:
                lines1 = f1.readlines()
                lines2 = f2.readlines()
        except UnicodeDecodeError:
             # Fallback for binary files if any (though these should be text)
            if filecmp.cmp(out_file, ref_file):
                 print(f"[PASS] {out_file} matches {ref_file}")
                 continue
            else:
                 print(f"[FAIL] {out_file} differs from {ref_file}")
                 all_passed = False
                 continue
            
        if lines1 == lines2:
            print(f"[PASS] {out_file} matches {ref_file}")
        else:
            print(f"[FAIL] {out_file} differs from {ref_file}")
            print("--- Diff Output ---")
            diff_cmd = ["diff", out_file, ref_file]
            subprocess.run(diff_cmd)
            print("-------------------")
            all_passed = False

    return all_passed

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python3 verify_sim.py <binary_file> <cache_config>")
        sys.exit(1)
        
    binary_file = sys.argv[1]
    cache_config = sys.argv[2]
    
    if verify_sim(binary_file, cache_config):
        print("\nVerification PASSED")
        sys.exit(0)
    else:
        print("\nVerification FAILED")
        sys.exit(1)
