global _start

section .note.openbsd.ident
        align   2
        dd      8
        dd      4
        dd      1
        db      "OpenBSD", 0
        dd      0
        align   2

section .text
_start:
        bits    32
        push    dword 14
        push    dword hello
        push    dword 1
        push    dword 0
        mov     eax, 4
        int     0x80
        push    dword 42
        push    dword 0
        mov     eax, 1
        int     0x80
        jmp     $

section .data
hello:
        db "Hello, world!", 10
