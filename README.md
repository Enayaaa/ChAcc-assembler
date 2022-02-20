# ChAcc Assembler
I wanted to write assembly to test on our processor made in VHDL and needed an assembler.
It was also an exercise to see how functional/combinatory parsing worked in haskell (see Parser.hs)

## Processor description
The ChAcc processor, based on the lab processor of HY-120 course in the institute of Computer Science
in FORTH, Greece, is a simple and slow processor which can run various programs. It is an 8-bit processor,
i.e., the processor executes operations on 8-bit data but executes instructions
which are 12-bit long. ChAcc makes use of the accumulator architecture, which has a special register, called
Accumulator (ACC). The register is so named because it can perform consecutive operations (e.g., additions)
and accumulate the result. ACC keeps the result of the most recent operation. Almost every instruction
works on ACC and the content of a memory location.

## Build
    ghc --make Main.hs

## Run
    ./Main valid.asm
