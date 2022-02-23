// this file explains the contents of memory.mif for lab 4
// you can use this file when your design does not complete the testbench 
// to check if every instruction is executed correctly

FCB     0xaa // hello world

org     0x02
ADD     ACC, M[64]       // ACC = ACC + M[64]
noop
ds
fcb     0x55

org     0x10
LB      ACC, M[65]        // ACC = M[65]
ADD     ACC, M[64]       // ACC = ACC + M[64]
ds                   // Display Reg <= ACC
SB      M[65], ACC        // DM[65] = acc
SUB     ACC, M[64]       // ACC = ACC - M[64]
SB      M[64], ACC        // DM[64] = ACC;
JA      0x00              // JUMP to address 0x00


j       0b10000000
