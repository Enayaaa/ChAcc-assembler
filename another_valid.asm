// Test program for lab 3
// Values provided below are in decimal

NOOP
ADD ACC, M[65]    // ACC = 0 + 7
SUB ACC, M[66]    // ACC = 7 - 2 = 5
SB M[67], ACC     // M[67] = 5
LB ACC, M[73]     // ACC = 245
NOT ACC           // ACC = 10
AND ACC, M[67]    // ACC = 10 AND 5 = 0
NOT ACC           // ACC = NOT ACC = 255
SBI M[M[68]], ACC // M[M[68]]= M[7] = 255
LBI ACC, M[M[69]] // ACC = M[M[69]] = M[67] = 5
ADD ACC, M[71]    // ACC = 5 + M[71] = 5 + 255 = 4 (overflow = 1)
CMP ACC, M[72]    // ACC = 4, M[72] = 4; --> EQ = 1
JNE 114           // If NEQ = 1, then jump with offset +115
IN M[70], IO_BUS  // externalIn (extIn) was set to 7 in the testbench, so that M[70] = 7
LB ACC, M[70]     // ACC = 7
CMP ACC, M[65]    // ACC = 7, M[65] = 7; --> EQ = 1, NEQ = 0
JEQ 1             // If EQ = 1, then jump with offset +2
SB M[64], ACC     // M[64] = ACC = 7
JA 128            // Jump to address 129
SB M[66], ACC    // DM[66] = 7
//.
//.
//.
JA 128            // Jump for ever
