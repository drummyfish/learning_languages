section   .text
global    _start         ; for linker

_start:
    mov   edx, strlen
    mov   ecx, msg_str
    mov   ebx, 1         ; stdout file descriptor
    mov   eax, 4         ; sys_write syscall number
    int   0x80           ; interrput - makes the system call

    mov   eax,1          ; sys_exit syscall number
    int   0x80           ; interrput - makes the system call

section   .data

msg_str   db "Hello, world!", 0xa  
strlen    equ $ - msg_str
