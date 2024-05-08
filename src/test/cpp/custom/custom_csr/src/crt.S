.globl _start
_start:

//Test 1
    li x28, 1
    csrr x1, 0xB08
    li x2, 3333
    bne x1, x2, fail

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
