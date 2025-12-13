li   t0, 5
li   t1, 60
sw   t0, 0(t1)
lw   t1, 0(t1)
bge  t1, zero, branch
li   t2, 2
branch: 
add zero, zero, zero
.word 0xfeedfeed


# Written by Adrian Tiripa, Christian Ganter, Eren Umay