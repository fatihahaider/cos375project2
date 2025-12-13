_start:
    li   t0, 0x2c         # t0 = &N 
    lw   t1, 0(t0)        # t1 = N
    li   t2, 0            # t2 = sum = 0
    li   t3, 1            # t3 = counter = 1

loop:
    add  t2, t2, t3       # sum += counter
    addi t3, t3, 1        # counter++
    bgt  t3, t1, done     # if counter > N BRANCH done
    j    loop

done:
    li   t0, 104          # t0 = &result
    sw   t2, 0(t0)        # store result
    .word 0xfeedfeed

N:       .word 10        # input N = 10
result:  .word 0         # space for result
