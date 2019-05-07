extern _printf
extern _time
extern _getchar

global _start
global doTheThing
global theMeaningOfLifeTheUniverseAndEverything
global main

section .data
    doTheThing__static__0: db 78,111,114,109,97,108,32,102,117,110,99,116,105,111,110,45,99,97,108,108,32,115,121,110,116,97,120,37,115,10,0
    doTheThing__static__1: db 46,0
    doTheThing__static__2: db 72,101,114,101,32,105,115,32,97,32,110,117,109,98,101,114,58,32,37,100,10,0
    main__static__0: db 84,104,101,32,116,105,109,101,32,105,115,58,32,37,100,10,65,110,100,32,104,101,114,101,32,105,115,32,116,104,101,32,118,97,108,117,101,32,111,102,32,97,32,108,111,99,97,108,32,118,97,114,105,97,98,108,101,58,32,37,100,10,0
    main__static__1: db 37,99,37,99,37,99,37,99,37,99,37,99,37,99,33,10,0

section .text
    _start:
        sub rsp, 8
        call main
        mov rax, 0x02000001
        mov rdi, 0
        syscall
    doTheThing:
        sub rsp, 72
        mov [rsp + 64], rdi ; param: n
        mov rax, doTheThing__static__0
        mov [rsp + 48], rax
        mov rax, doTheThing__static__1
        mov [rsp + 40], rax
        mov rdi, [rsp + 48]
        mov rsi, [rsp + 40]
        call _printf
        mov [rsp + 32], rax
        mov rax, doTheThing__static__2
        mov [rsp + 24], rax
        mov rax, [rsp + 64] ; param: n
        mov [rsp + 16], rax
        mov rdi, [rsp + 24]
        mov rsi, [rsp + 16]
        call _printf
        mov [rsp + 8], rax
        mov rax, 17
        mov [rsp + 0], rax
        mov rax, [rsp + 0]
        jmp doTheThing__end
        doTheThing__end:
        add rsp, 72
        ret
    theMeaningOfLifeTheUniverseAndEverything:
        sub rsp, 8
        mov rax, 42
        mov [rsp + 0], rax
        mov rax, [rsp + 0]
        jmp theMeaningOfLifeTheUniverseAndEverything__end
        theMeaningOfLifeTheUniverseAndEverything__end:
        add rsp, 8
        ret
    main:
        sub rsp, 168
        mov rax, 123
        mov [rsp + 160], rax
        mov rax, [rsp + 160]
        mov [rsp + 152], rax
        call theMeaningOfLifeTheUniverseAndEverything
        mov [rsp + 144], rax
        mov rdi, [rsp + 144]
        call doTheThing
        mov [rsp + 136], rax
        mov rdi, [rsp + 136]
        call doTheThing
        mov [rsp + 128], rax
        mov rax, main__static__0
        mov [rsp + 120], rax
        mov rax, 0
        mov [rsp + 112], rax
        mov rdi, [rsp + 112]
        call _time
        mov [rsp + 104], rax
        mov rax, [rsp + 152] ; local: xyz
        mov [rsp + 96], rax
        mov rdi, [rsp + 120]
        mov rsi, [rsp + 104]
        mov rdx, [rsp + 96]
        call _printf
        mov [rsp + 88], rax
        call _getchar
        mov [rsp + 80], rax
        mov rax, [rsp + 80]
        mov [rsp + 72], rax
        mov rax, main__static__1
        mov [rsp + 64], rax
        mov rax, 103
        mov [rsp + 56], rax
        mov rax, 111
        mov [rsp + 48], rax
        mov rax, 111
        mov [rsp + 40], rax
        mov rax, 100
        mov [rsp + 32], rax
        mov rax, 98
        mov [rsp + 24], rax
        mov rax, 121
        mov [rsp + 16], rax
        mov rax, 101
        mov [rsp + 8], rax
        mov rdi, [rsp + 64]
        mov rsi, [rsp + 56]
        mov rdx, [rsp + 48]
        mov rcx, [rsp + 40]
        mov r8, [rsp + 32]
        mov r9, [rsp + 24]
        push qword [rsp + 8]
        push qword [rsp + 16]
        call _printf
        pop qword rax
        pop qword rax
        mov [rsp + 0], rax
        main__end:
        add rsp, 168
        ret

