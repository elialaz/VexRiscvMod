.globl _start
_start:

//Test 1
    li x28, 1
    csrw 0xB09, x1 // Write to the 0xB09 register
    csrr x1, 0xB08 // Read the 0xB08 register
    li x2, 0
    bne x1, x2, fail // If the value read from the 0xB08 register is not equal to 5, jump to fail

    j pass

fail: //x28 => error code
    li x2, 0xF00FFF24
    sw x28, 0(x2)

pass:
    li x2, 0xF00FFF20
    sw x0, 0(x2)



    nop
    nop
    nop
    nop
    nop
    nop
