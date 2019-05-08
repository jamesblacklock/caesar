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
    main__static__1: db 89,111,117,32,116,121,112,101,100,32,116,104,101,32,108,101,116,116,101,114,32,39,97,39,46,10,0
    main__static__2: db 89,111,117,32,100,105,100,32,110,111,116,32,116,121,112,101,32,116,104,101,32,108,101,116,116,101,114,32,39,97,39,46,32,89,111,117,32,116,121,112,101,100,32,39,37,99,39,10,0
    main__static__3: db 37,99,37,99,37,99,37,99,37,99,37,99,37,99,33,10,0

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
        sub rsp, 376
        mov rax, 123
        mov [rsp + 368], rax
        mov rax, [rsp + 368]
        mov [rsp + 360], rax
        call theMeaningOfLifeTheUniverseAndEverything
        mov [rsp + 352], rax
        mov rdi, [rsp + 352]
        call doTheThing
        mov [rsp + 344], rax
        mov rdi, [rsp + 344]
        call doTheThing
        mov [rsp + 336], rax
        mov rax, main__static__0
        mov [rsp + 328], rax
        mov rax, 0
        mov [rsp + 320], rax
        mov rdi, [rsp + 320]
        call _time
        mov [rsp + 312], rax
        mov rax, [rsp + 360] ; local: xyz
        mov [rsp + 304], rax
        mov rdi, [rsp + 328]
        mov rsi, [rsp + 312]
        mov rdx, [rsp + 304]
        call _printf
        mov [rsp + 296], rax
        call _getchar
        mov [rsp + 288], rax
        mov rax, [rsp + 288]
        mov [rsp + 280], rax
        mov rax, [rsp + 280] ; local: c
        mov [rsp + 272], rax
        mov rax, 4
        mov [rsp + 264], rax
        mov rax, 3
        mov [rsp + 256], rax
        mov rax, [rsp + 264]
        mov rbx, [rsp + 256]
        mul rbx
        mov [rsp + 248], rax
        mov rax, [rsp + 272]
        add rax, [rsp + 248]
        mov [rsp + 240], rax
        mov rax, 2
        mov [rsp + 232], rax
        mov rax, [rsp + 240]
        add rax, [rsp + 232]
        mov [rsp + 224], rax
        mov rax, 13
        mov [rsp + 216], rax
        mov rax, [rsp + 224]
        sub rax, [rsp + 216]
        mov [rsp + 208], rax
        mov rax, 98
        mov [rsp + 200], rax
        mov rbx, 0
        mov rax, [rsp + 208]
        cmp rax, [rsp + 200]
        setz bl
        mov [rsp + 192], rbx
        mov rax, [rsp + 192]
        cmp rax, 0
        jz main__elselabel__0
        mov rax, main__static__1
        mov [rsp + 184], rax
        mov rdi, [rsp + 184]
        call _printf
        mov [rsp + 176], rax
        jmp main__endiflabel__1
        main__elselabel__0:
        mov rax, main__static__2
        mov [rsp + 168], rax
        mov rax, [rsp + 280] ; local: c
        mov [rsp + 160], rax
        mov rax, 10
        mov [rsp + 152], rax
        mov rax, 2
        mov [rsp + 144], rax
        mov rax, [rsp + 152]
        mov rbx, [rsp + 144]
        mul rbx
        mov [rsp + 136], rax
        mov rax, [rsp + 160]
        add rax, [rsp + 136]
        mov [rsp + 128], rax
        mov rax, 11
        mov [rsp + 120], rax
        mov rax, 2
        mov [rsp + 112], rax
        mov rax, [rsp + 120]
        mov rbx, [rsp + 112]
        mul rbx
        mov [rsp + 104], rax
        mov rax, [rsp + 128]
        sub rax, [rsp + 104]
        mov [rsp + 96], rax
        mov rax, 2
        mov [rsp + 88], rax
        mov rax, [rsp + 96]
        add rax, [rsp + 88]
        mov [rsp + 80], rax
        mov rdi, [rsp + 168]
        mov rsi, [rsp + 80]
        call _printf
        mov [rsp + 72], rax
        main__endiflabel__1:
        mov rax, main__static__3
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
        add rsp, 376
        ret
