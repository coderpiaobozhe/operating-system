org 0x7C00
BaseOfStack equ 0x7C00
BaseOfLoader equ 0x1000
OffsetOfLoader equ 0x00
mov ax, cs
mov ds, ax
mov es, ax
mov ss, ax
mov sp, BaseOfStack
;----- clear screen
mov ax, 0600h
mov bx, 0700h
mov cx, 0
mov dx, 0184Fh
int 10h
;----- set focus
mov ax, 0200h
mov bx, 0
mov dx, 0
int 10h
;----- reset ax
xor ax, ax
;----- display string
mov si, BootMsg
call Func_Print
;----- Get Loader
mov edi, 0x1000;target memory address
mov ecx, 2;third sector
mov bl, 4;amount of sectors
call Func_Read
cmp word [0x1000], 0xAA55
jnz Error
jmp 0:0x1002
;----- block
jmp $

Func_Print:
    push ax
    mov ah, 0xE
    .next:
        mov al, [si]
        cmp al, 0
        jz .return
        int 10h
        inc si
        jmp .next
    .return:
        pop ax
        ret
; LBA Mode
Func_Read:
    push ecx
    push dx
    push ax
    ;set the amount of sectors
    mov dx, 0x1F2
    mov al, bl
    out dx, al 
    ; 0x1F3
    inc dx
    mov al, cl
    out dx, al
    ; 0x1F4
    inc dx
    shr ecx, 8
    mov al, cl
    out dx, al
    ; 0x1F5
    inc dx
    shr ecx, 8
    mov al, cl
    out dx, al
    ; 0x1F6
    inc dx
    shr ecx, 8
    mov al, 0b1110_0000
    and cl, 0b0000_1111
    or al, cl
    out dx, al
    ; 0x1F7(read data from disk)
    inc dx
    mov al, 0x20
    out dx, al 

    xor ecx, ecx
    mov cl, bl
    .read:
        ;check if the disk is ready
        mov dx, 0x1F7
        .ready:
            in al, dx
            jmp $+2
            jmp $+2
            jmp $+2
            and al, 0b1000_1000
            cmp al, 0b0000_1000
            jnz .ready
        ;read data from the disk
        mov dx, 0X1F0
        push cx
        mov cx, 256 ; one sector contains 256 words
        .reading:
            in ax, dx
            jmp $+2
            jmp $+2
            jmp $+2
            mov [edi], ax
            add edi, 2
            loop .reading
        pop cx
        loop .read
    pop ax
    pop dx
    pop ecx
    ret

BootMsg: db "Begin to boot", 10, 13, 0
Error:
    mov si, .errmsg
    call Func_Print
    hlt
    .errmsg db "Error : Failed to enter the loader program", 10, 13, 0
times 510 - ($ - $$) db 0
;----- 小端编址，先55再aa
dw 0xAA55 