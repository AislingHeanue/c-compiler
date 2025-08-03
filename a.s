    .globl main
    .text
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $48, %rsp
    movl $10, -4(%rbp)
    leaq -4(%rbp), %r11
    movq %r11, -16(%rbp)
    movq -16(%rbp), %r10
    movq %r10, -24(%rbp)
    movq -24(%rbp), %rax
    movl 0(%rax), %r10d
    movl %r10d, -28(%rbp)
    movl -28(%rbp), %r10d
    movl %r10d, -32(%rbp)
    addl $1, -32(%rbp)
    movq -24(%rbp), %rax
    movl -32(%rbp), %r10d
    movl %r10d, 0(%rax)
    cmpl $11, -32(%rbp)
    movl $0, -36(%rbp)
    jne .Lnot_nan_1
    jae .Lnot_nan_1
    jp .Lnan_1
    jmp .Lnot_nan_1
.Lnan_1:
    movl $1, %r11d
    cmpl $0, %r11d
.Lnot_nan_1:
    setne -36(%rbp)
    cmpl $0, -36(%rbp)
    jne .Lnot_nan_2
    jae .Lnot_nan_2
    jp .Lnan_2
    jmp .Lnot_nan_2
.Lnan_2:
    movl $0, %r11d
    cmpl $1, %r11d
.Lnot_nan_2:
    je .Lend_1
    movl $1, %eax
    movq %rbp, %rsp
    popq %rbp
    ret
.Lend_1:
    cmpl $11, -4(%rbp)
    movl $0, -40(%rbp)
    jne .Lnot_nan_3
    jae .Lnot_nan_3
    jp .Lnan_3
    jmp .Lnot_nan_3
.Lnan_3:
    movl $1, %r11d
    cmpl $0, %r11d
.Lnot_nan_3:
    setne -40(%rbp)
    cmpl $0, -40(%rbp)
    jne .Lnot_nan_4
    jae .Lnot_nan_4
    jp .Lnan_4
    jmp .Lnot_nan_4
.Lnan_4:
    movl $0, %r11d
    cmpl $1, %r11d
.Lnot_nan_4:
    je .Lend_2
    movl $2, %eax
    movq %rbp, %rsp
    popq %rbp
    ret
.Lend_2:
    movl $0, %eax
    movq %rbp, %rsp
    popq %rbp
    ret

    .section .note.GNU-stack,"",@progbits

